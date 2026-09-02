# Changelog

Notable changes to Kater will be documented in this file.  This
repository is only updated whenever a new version of Kater is
released.

## [Unreleased]

## [0.4.0] - 2026.09.02
### Added

- A number of performance optimizations, including antichain-based inclusion checking
- `cmake --install` now installs the `kater` binary
- Better diagnostics for code export
- Developer documentation (see `doc/development.md`) and clang-tidy config

### Changed

- Changed code export (e.g., for user-defined relations)
- A C++23 compiler is required
- flex/bison sources are generated out of tree

### Fixes

- Many bug fixes
- Properly resolve diamond and cyclic include paths

## [0.3.1] - 2025.11.11
### Changed

- Code export for GenMC updated (v0.14.1)

## [0.3] - 2024.09.03
### Added

- Support for mutually recursive relations via `let rec`
- Support for `extra` acyclicity exports

### Changed

- Using immediate relations (e.g., `po-imm`, `mo-imm`, etc) is now forbidden
- Different code export for GenMC

### Fixes

- Various bug fixes (e.g., for assertion checking)

## [0.2] - 2024.02.12
### Added

- Support for user-declared predicates via `predicate`
- Support for declaring pairwise disjoint predicates via `disjoint`
- Performance improvements
- clang-format file for project

### Changed

- User relations are now declared with `relation`
- Non-standard assumptions need to be of the form `[A];po;[B] <= [A];po;[F];po;[B]`
- The consistency-checking clauses of a KAT file now require the `export` prefix
- C++20 features are required for compilation
- Switch to `cmake` (instead of `autotools`)

### Fixes

- Various bug fixes
- Documentation fixes

## [0.1] - 2023.07.25
### Added

- Release on Github
