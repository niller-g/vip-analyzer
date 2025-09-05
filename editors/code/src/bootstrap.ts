import * as vscode from "vscode";
import * as os from "os";
import type { Config } from "./config";
import { type Env, log, spawnAsync } from "./util";
import type { PersistentState } from "./persistent_state";

export async function bootstrap(
    context: vscode.ExtensionContext,
    config: Config,
    state: PersistentState,
): Promise<string> {
    const path = await getServer(context, config, state);
    if (!path) {
        throw new Error(
            "VIP Analyzer Language Server is not available. Please, ensure its proper installation.",
        );
    }

    log.info("Using server binary at", path);

    if (!isValidExecutable(path, config.serverExtraEnv)) {
        throw new Error(
            `Failed to execute ${path} --version.` +
                (config.serverPath
                    ? `\`config.server.path\` or \`config.serverPath\` has been set explicitly.\
            Consider removing this config or making a valid server binary available at that path.`
                    : ""),
        );
    }

    return path;
}
async function getServer(
    context: vscode.ExtensionContext,
    config: Config,
    state: PersistentState,
): Promise<string | undefined> {
    const packageJson: {
        version: string;
        releaseTag: string | null;
        enableProposedApi: boolean | undefined;
    } = context.extension.packageJSON;

    // check if the server path is configured explicitly
    const explicitPath = process.env["__VA_LSP_SERVER_DEBUG"] ?? config.serverPath;
    if (explicitPath) {
        if (explicitPath.startsWith("~/")) {
            return os.homedir() + explicitPath.slice("~".length);
        }
        return explicitPath;
    }

    if (packageJson.releaseTag === null) return "vip-analyzer";

    // finally, use the bundled one
    const ext = process.platform === "win32" ? ".exe" : "";
    const bundled = vscode.Uri.joinPath(context.extensionUri, "server", `vip-analyzer${ext}`);
    const bundledExists = await fileExists(bundled);
    if (bundledExists) {
        await state.updateServerVersion(packageJson.version);
        return bundled.fsPath;
    }

    await vscode.window.showErrorMessage(
        "Unfortunately we couldn't find a suitable VIP Analyzer Language Server binary.",
    );
    return undefined;
}

async function fileExists(uri: vscode.Uri) {
    return await vscode.workspace.fs.stat(uri).then(
        () => true,
        () => false,
    );
}

export async function isValidExecutable(path: string, extraEnv: Env): Promise<boolean> {
    log.debug("Checking availability of a binary at", path);

    const newEnv = { ...process.env };
    for (const [k, v] of Object.entries(extraEnv)) {
        if (v) {
            newEnv[k] = v;
        } else if (k in newEnv) {
            delete newEnv[k];
        }
    }
    const res = await spawnAsync(path, ["--version"], {
        env: newEnv,
    });

    if (res.error) {
        log.warn(path, "--version:", res);
    } else {
        log.info(path, "--version:", res);
    }
    return res.status === 0;
}
