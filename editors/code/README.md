# vip-analyzer

This extension provides support for the [Visual Prolog programming language](https://www.visual-prolog.com/).

## Features

- [x] Multi project workspace
- [x] Syntax highlighting
- [x] Syntax checking
- [x] Code formatting (via VIP formatter)
- [x] Diagnostics (via VIP compiler)
- Goto definition
    - [x] Goto definition on constructs with explicit types. E.g. `string::concat(A, B)`.
    - [ ] Goto definition on constructs with inferred types. E.g. `Opt:isSome()`.
- Goto declaration
    - [x] Goto declaration on constructs with explicit types. E.g. `string::concat(A, B)`.
    - [ ] Goto declaration on constructs with inferred types. E.g. `Opt:isSome()`.
- [ ] Find references
- [ ] Code completions
- [ ] Refactorings
- ..

## Quick start

1. Install the `vip-analyzer` extension.
2. Open the folder containing your Visual Prolog project(s) (\*.vipprj files).

    ```plaintext
    /src
    ├── server
    │   ├── server.vipprj
    │   ├── main.pack
    │   └── ...
    └── client
        ├── client.vipprj
        ├── main.pack
        └── ...
    ```

    **Note: For multi-project workspaces**

    - [Shadow files](https://wiki.visual-prolog.com/index.php?title=Shadowing_PFC_files) can introduce ambiguity in e.g. goto definition/declaration requests.
    - The build tool `VipBuilder.exe` might overload the CPU and RAM when building many projects at once.

3. Get coding!

## Configuration

This extension provides configurations through VSCode's configuration settings. All configurations are under `vip-analyzer.*`.

## Recommended configuration

### Formatting

- Enable formatting on save in [user settings](https://code.visualstudio.com/docs/configure/settings#_user-settings) (you can choose to make it language specific).

```json
{
    "editor.formatOnSave": true
}
```

### [Keybindings](https://code.visualstudio.com/docs/configure/keybindings)

- Go to declaration (`editor.action.revealDeclaration`) suggestion: `ctrl+shift+f12` or `ctrl+d`.

- Navigate back (`workbench.action.navigateBack`) suggestion: `ctrl+shift+f8` or keep the default `alt+LeftArrow`.

### [Workspace](https://code.visualstudio.com/docs/configure/settings#_workspace-settings)

- It is easy to include directories from outside the workspace. This is useful e.g. for bringing library code into the explorer such as `pfc`. To do this, follow [this guide](https://code.visualstudio.com/docs/editing/workspaces/multi-root-workspaces#_adding-folders).

- Configure `files.exclude`, `explorer.excludeGitIgnore`, and `search.exclude` user setting to your desired folders.
  Otherwise you might be missing files and folders in the explorer and when performing text searches.
  It is common to that VSCode excludes `node_modules` however this is often relevant in the context of Visual Prolog projects.
