import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";

import * as commands from "./commands";
import { type CommandFactory, Ctx, fetchWorkspace } from "./ctx";
import * as diagnostics from "./diagnostics";
import { setContextValue } from "./util";

const VIP_PROJECT_CONTEXT_NAME = "inVipProject";

export interface VipAnalyzerExtensionApi {
    // FIXME: this should be non-optional
    readonly client?: lc.LanguageClient;
}

export async function deactivate() {
    await setContextValue(VIP_PROJECT_CONTEXT_NAME, undefined);
}

export async function activate(context: vscode.ExtensionContext): Promise<VipAnalyzerExtensionApi> {
    checkConflictingExtensions();

    const ctx = new Ctx(context, createCommands(), fetchWorkspace());
    // VS Code doesn't show a notification when an extension fails to activate
    // so we do it ourselves.
    const api = await activateServer(ctx).catch((err) => {
        void vscode.window.showErrorMessage(
            `Cannot activate vip-analyzer extension: ${err.message}`,
        );
        throw err;
    });
    await setContextValue(VIP_PROJECT_CONTEXT_NAME, true);
    return api;
}

async function activateServer(ctx: Ctx): Promise<VipAnalyzerExtensionApi> {
    // if (ctx.workspace.kind === "Workspace Folder") {
    //     ctx.pushExtCleanup(activateTaskProvider(ctx.config));
    // }

    const diagnosticProvider = new diagnostics.TextDocumentProvider(ctx);
    ctx.pushExtCleanup(
        vscode.workspace.registerTextDocumentContentProvider(
            diagnostics.URI_SCHEME,
            diagnosticProvider,
        ),
    );

    const decorationProvider = new diagnostics.AnsiDecorationProvider(ctx);
    ctx.pushExtCleanup(decorationProvider);

    async function decorateVisibleEditors(document: vscode.TextDocument) {
        for (const editor of vscode.window.visibleTextEditors) {
            if (document === editor.document) {
                await decorationProvider.provideDecorations(editor);
            }
        }
    }

    vscode.workspace.onDidChangeTextDocument(
        async (event) => await decorateVisibleEditors(event.document),
        null,
        ctx.subscriptions,
    );
    vscode.workspace.onDidOpenTextDocument(decorateVisibleEditors, null, ctx.subscriptions);
    vscode.window.onDidChangeActiveTextEditor(
        async (editor) => {
            if (editor) {
                diagnosticProvider.triggerUpdate(editor.document.uri);
                await decorateVisibleEditors(editor.document);
            }
        },
        null,
        ctx.subscriptions,
    );
    vscode.window.onDidChangeVisibleTextEditors(
        async (visibleEditors) => {
            for (const editor of visibleEditors) {
                diagnosticProvider.triggerUpdate(editor.document.uri);
                await decorationProvider.provideDecorations(editor);
            }
        },
        null,
        ctx.subscriptions,
    );

    vscode.workspace.onDidChangeWorkspaceFolders(
        async (_) => ctx.onWorkspaceFolderChanges(),
        null,
        ctx.subscriptions,
    );
    vscode.workspace.onDidChangeConfiguration(
        async (_) => {
            await ctx.client?.sendNotification(lc.DidChangeConfigurationNotification.type, {
                settings: "",
            });
        },
        null,
        ctx.subscriptions,
    );

    if (ctx.config.initializeStopped) {
        ctx.setServerStatus({
            health: "stopped",
        });
    } else {
        await ctx.start();
    }

    return ctx;
}

function createCommands(): Record<string, CommandFactory> {
    return {
        onEnter: {
            enabled: commands.onEnter,
            disabled: (_) => () => vscode.commands.executeCommand("default:type", { text: "\n" }),
        },
        restartServer: {
            enabled: (ctx) => async () => {
                await ctx.restart();
            },
            disabled: (ctx) => async () => {
                await ctx.start();
            },
        },
        startServer: {
            enabled: (ctx) => async () => {
                await ctx.start();
            },
            disabled: (ctx) => async () => {
                await ctx.start();
            },
        },
        stopServer: {
            enabled: (ctx) => async () => {
                // FIXME: We should re-use the client, that is ctx.deactivate() if none of the configs have changed
                await ctx.stopAndDispose();
                ctx.setServerStatus({
                    health: "stopped",
                });
            },
            disabled: (_) => async () => {},
        },

        analyzerStatus: { enabled: commands.analyzerStatus },
        // memoryUsage: { enabled: commands.memoryUsage },
        reloadWorkspace: { enabled: commands.reloadWorkspace },
        joinLines: { enabled: commands.joinLines },
        parentModule: { enabled: commands.parentModule },
        viewFileText: { enabled: commands.viewFileText },
        // debug: { enabled: commands.debug },
        openDocs: { enabled: commands.openDocs },
        openExternalDocs: { enabled: commands.openExternalDocs },
        moveItemUp: { enabled: commands.moveItemUp },
        moveItemDown: { enabled: commands.moveItemDown },
        cancelFlycheck: { enabled: commands.cancelFlycheck },
        clearFlycheck: { enabled: commands.clearFlycheck },
        runFlycheck: { enabled: commands.runFlycheck },
        ssr: { enabled: commands.ssr },
        serverVersion: { enabled: commands.serverVersion },
        viewMemoryLayout: { enabled: commands.viewMemoryLayout },
        toggleCheckOnSave: { enabled: commands.toggleCheckOnSave },
        toggleLSPLogs: { enabled: commands.toggleLSPLogs },
        openWalkthrough: { enabled: commands.openWalkthrough },
        // Internal commands which are invoked by the server.
        applyActionGroup: { enabled: commands.applyActionGroup },
        applySnippetWorkspaceEdit: { enabled: commands.applySnippetWorkspaceEditCommand },
        gotoLocation: { enabled: commands.gotoLocation },
        linkToCommand: { enabled: commands.hoverRefCommandProxy },
        resolveCodeAction: { enabled: commands.resolveCodeAction },
        showReferences: { enabled: commands.showReferences },
        triggerParameterHints: { enabled: commands.triggerParameterHints },
        rename: { enabled: commands.rename },
        openLogs: { enabled: commands.openLogs },
        syntaxTreeReveal: { enabled: commands.syntaxTreeReveal },
        syntaxTreeCopy: { enabled: commands.syntaxTreeCopy },
        syntaxTreeHideWhitespace: { enabled: commands.syntaxTreeHideWhitespace },
        syntaxTreeShowWhitespace: { enabled: commands.syntaxTreeShowWhitespace },
    };
}

function checkConflictingExtensions() {
    if (
        vscode.extensions.all.find((ext) => {
            return ext.id.endsWith(".visual-prolog-syntax-highlight");
        })
    ) {
        vscode.window
            .showWarningMessage(
                `You have the Visual Prolog Syntax Highlight extension installed. ` +
                    "This extension is known to conflict with vip-analyzer and may cause " +
                    "various functions of vip-analyzer to not work correctly. You should disable it.",
                "Got it",
            )
            // eslint-disable-next-line no-console
            .then(() => {}, console.error);
    }
}
