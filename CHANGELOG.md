# Changelog

Notable changes to Kater will be documented in this file.  This
repository is only updated whenever a new version of Kater is
released.

## [Unreleased]

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
