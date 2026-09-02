# Building Kater

Kater needs a C++23 compiler (e.g., GCC >= 14), CMake, flex (>= 2.6) and bison
(>= 3.2). On a Debian-based system:

```shell
apt-get install cmake make g++ flex bison libfl-dev
```

For a release build:

```shell
cmake -DCMAKE_BUILD_TYPE=Release -B Release -S .
cmake --build Release -j$(nproc)
```

This leaves the `kater` executable in `Release/`.

## Build options

 - `KATER_DEBUG`: enable Kater's debugging infrastructure (the `KATER_DEBUG`/
   `DEBUG_WITH_TYPE` logging macros). Enabled by default for `Debug` builds, off
   otherwise.
 - `TIDY`: run `clang-tidy` as part of the build (wires `CMAKE_CXX_CLANG_TIDY`
   with the repo's `.clang-tidy`).

A `compile_commands.json` is always emitted (see *Editor setup* below).

# Testing Kater

## Running tests

Run the test suite (the `tests/correct` and `tests/wrong` cases):

```shell
cd scripts && ./checkall.sh
```

Environment variables affecting the testing scripts:

```shell
  KATER="path/to/kater"          # kater executable to test (default: Release/kater)
  KATERFLAGS="--flag --another"  # extra flags passed to every kater invocation
```

Other helpers under `scripts/`:

 - `check-drivers.sh`: exports GenMC consistency checkers from the `kat/` models
   and builds/tests them against a GenMC checkout (`KATER`, `GENMC`, `DRIVERS`).
 - `check-regression.sh`: compares performance between two refs (used on MRs).

# Modifying Kater

## Pull requests

Please submit PRs as a series of atomic, easily reversible commits. Code must be
formatted with `clang-format` and free of `clang-tidy` warnings on the diff. Run:

```shell
./scripts/lint.sh           # uses the RelWithDebInfo build dir by default
```

`lint.sh` checks `clang-format` over all of `src/` and runs `clang-tidy` on the
changes relative to the merge base. Both checks gate CI.

## Code conventions

### Comments

Please use `/*` and `*/` as comment delimiters.

### Classes

- Declaration order within a class (public -> protected -> private):
  1. Types and type aliases
  2. Static constants/functions (e.g., factories)
  3. Ctors, assignment operators and dtor
  4. All other functions (ending with operators and friends)
  5. Data members (static -> nonstatic)

### Naming

- CamelCase for everything apart from things mimicking STL functionality
- Names start with a lowercase letter
- Member variables should end with an underscore (`_`)

### Assertions

Use `VERIFY(cond[, msg])` (`src/Error.hpp`) where breaking the invariant would
corrupt results silently, e.g. an ID used to index a vector: it stays on in
release builds, and under `KATER_DEBUG` it offers to attach a debugger. Use plain
`assert` elsewhere, and for checks whose cost grows with the automaton.

### Diagnostics

Build with `-DKATER_DEBUG=ON` to enable Kater's debug infrastructure. Use
`KATER_DEBUG(X)` / `DEBUG_WITH_TYPE(type, X)` (`src/Error.hpp`) for category-gated
debug output, and plain `assert` for invariants.
