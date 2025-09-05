<p align="center">
  <img
    src="editors/code/icon.png" width="100" height="100"
    alt="vip-analyzer logo">
</p>

[![.github/workflows/ci.yml](https://github.com/niller-g/vip-analyzer/actions/workflows/ci.yml/badge.svg)](https://github.com/niller-g/vip-analyzer/actions/workflows/ci.yml)

vip-analyzer is a compiler frontend for the [Visual Prolog language](https://www.visual-prolog.com/).
The implementation is fully based on the [rust-analyzer](https://github.com/rust-lang/rust-analyzer), and has been kept up-to-date until commit `f8e784353bde` in the [rust-analyzer](https://github.com/rust-lang/rust-analyzer) repository.

### Quick Start

Both the server and the Code plugin can be installed from source:

```bash
cargo xtask install
```

You’ll need Cargo, nodejs (matching a supported version of VS Code) and npm for this.

### Features

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

### Contributing

If you want to **contribute** to vip-analyzer check out the [CONTRIBUTING.md](./CONTRIBUTING.md)

### License

vip-analyzer is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
