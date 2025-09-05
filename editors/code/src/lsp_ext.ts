/**
 * This file mirrors `crates/vip-analyzer/src/lsp_ext.rs` declarations.
 */

import * as lc from "vscode-languageclient";

// vip-analyzer overrides

export const hover = new lc.RequestType<
    HoverParams,
    (lc.Hover & { actions: CommandLinkGroup[] }) | null,
    void
>(lc.HoverRequest.method);
export type HoverParams = { position: lc.Position | lc.Range } & Omit<lc.HoverParams, "position">;

export type CommandLink = {
    /**
     * A tooltip for the command, when represented in the UI.
     */
    tooltip?: string;
} & lc.Command;
export type CommandLinkGroup = {
    title?: string;
    commands: CommandLink[];
};

// vip-analyzer extensions

export const analyzerStatus = new lc.RequestType<AnalyzerStatusParams, string, void>(
    "vip-analyzer/analyzerStatus",
);
export const cancelFlycheck = new lc.NotificationType0("vip-analyzer/cancelFlycheck");
export const clearFlycheck = new lc.NotificationType0("vip-analyzer/clearFlycheck");
export const memoryUsage = new lc.RequestType0<string, void>("vip-analyzer/memoryUsage");
export const openServerLogs = new lc.NotificationType0("vip-analyzer/openServerLogs");
export const reloadWorkspace = new lc.RequestType0<null, void>("vip-analyzer/reloadWorkspace");

export const runFlycheck = new lc.NotificationType<{
    textDocument: lc.TextDocumentIdentifier | null;
}>("vip-analyzer/runFlycheck");
export const viewSyntaxTree = new lc.RequestType<ViewSyntaxTreeParams, string, void>(
    "vip-analyzer/viewSyntaxTree",
);
export const viewFileText = new lc.RequestType<lc.TextDocumentIdentifier, string, void>(
    "vip-analyzer/viewFileText",
);

export type AnalyzerStatusParams = { textDocument?: lc.TextDocumentIdentifier };

export type ViewSyntaxTreeParams = { textDocument: lc.TextDocumentIdentifier };

// experimental extensions

export const joinLines = new lc.RequestType<JoinLinesParams, lc.TextEdit[], void>(
    "experimental/joinLines",
);
export const moveItem = new lc.RequestType<MoveItemParams, lc.TextEdit[], void>(
    "experimental/moveItem",
);
export const onEnter = new lc.RequestType<lc.TextDocumentPositionParams, lc.TextEdit[], void>(
    "experimental/onEnter",
);
export interface DocsUrls {
    local?: string;
    web?: string;
}
export const openDocs = new lc.RequestType<lc.TextDocumentPositionParams, DocsUrls, void>(
    "experimental/externalDocs",
);
export const parentModule = new lc.RequestType<
    lc.TextDocumentPositionParams,
    lc.LocationLink[] | null,
    void
>("experimental/parentModule");
export const serverStatus = new lc.NotificationType<ServerStatusParams>(
    "experimental/serverStatus",
);
export const ssr = new lc.RequestType<SsrParams, lc.WorkspaceEdit, void>("experimental/ssr");
export const viewRecursiveMemoryLayout = new lc.RequestType<
    lc.TextDocumentPositionParams,
    RecursiveMemoryLayout | null,
    void
>("vip-analyzer/viewRecursiveMemoryLayout");

export type JoinLinesParams = {
    textDocument: lc.TextDocumentIdentifier;
    ranges: lc.Range[];
};
export type MatchingBraceParams = {
    textDocument: lc.TextDocumentIdentifier;
    positions: lc.Position[];
};
export type MoveItemParams = {
    textDocument: lc.TextDocumentIdentifier;
    range: lc.Range;
    direction: Direction;
};
export type Direction = "Up" | "Down";
export type OpenCargoTomlParams = {
    textDocument: lc.TextDocumentIdentifier;
};
export type ServerStatusParams = {
    health: "ok" | "warning" | "error";
    quiescent: boolean;
    message?: string;
};
export type SsrParams = {
    query: string;
    parseOnly: boolean;
    textDocument: lc.TextDocumentIdentifier;
    position: lc.Position;
    selections: readonly lc.Range[];
};

export type RecursiveMemoryLayoutNode = {
    item_name: string;
    typename: string;
    size: number;
    alignment: number;
    offset: number;
    parent_idx: number;
    children_start: number;
    children_len: number;
};
export type RecursiveMemoryLayout = {
    nodes: RecursiveMemoryLayoutNode[];
};
