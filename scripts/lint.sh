#!/bin/bash
#
# Runs clang-format and clang-tidy on a branch against a base
#
# Usage: ./scripts/lint.sh [build-dir]      # default build-dir: RelWithDebInfo
#
# Environment:
#   CLANG_FORMAT     clang-format binary       (default: clang-format)
#   CLANG_TIDY       clang-tidy binary         (default: clang-tidy)
#   CLANG_TIDY_DIFF  clang-tidy-diff.py script (default: auto-detected)
#   BASE             base git ref for the diff (default: the MR base in CI, else
#                    merge-base against origin/master)

set -uo pipefail

CLANG_FORMAT="${CLANG_FORMAT:-clang-format}"
CLANG_TIDY="${CLANG_TIDY:-clang-tidy}"

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "${DIR}/.." || exit 1

BUILD_DIR="${1:-RelWithDebInfo}"
CORES="$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)"
rc=0

# Exclude flex/bison generated files
mapfile -t SOURCES < <(git ls-files 'src/*.cpp' 'src/*.hpp' 'tests/*.cpp' 'tests/*.hpp')

# clang-format
echo "[lint] clang-format ($(${CLANG_FORMAT} --version 2>/dev/null))"
if ! printf '%s\n' "${SOURCES[@]}" | xargs "${CLANG_FORMAT}" --dry-run --Werror; then
	echo "[lint] clang-format: formatting issues found (run: clang-format -i <file>)"
	rc=1
fi

# Resolve the base ref the diff is taken against.
BASE="${BASE:-${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}}"
if [ -z "${BASE}" ]; then
	BASE="$(git merge-base origin/master HEAD 2>/dev/null \
		|| git merge-base master HEAD 2>/dev/null \
		|| echo HEAD~1)"
fi

# Locate clang-tidy-diff.py (Debian ships it as clang-tidy-diff-<v>.py).
find_tidy_diff() {
	[ -n "${CLANG_TIDY_DIFF:-}" ] && { echo "${CLANG_TIDY_DIFF}"; return; }
	local c
	for c in clang-tidy-diff.py clang-tidy-diff-21.py clang-tidy-diff-19.py; do
		command -v "$c" >/dev/null 2>&1 && { command -v "$c"; return; }
	done
	for c in /usr/lib/llvm-*/share/clang/clang-tidy-diff.py \
		 /usr/share/clang/clang-tidy-diff*.py; do
		[ -e "$c" ] && { echo "$c"; return; }
	done
}
TIDY_DIFF="$(find_tidy_diff)"

# clang-tidy (diff only)
echo "[lint] clang-tidy diff against ${BASE} ($(${CLANG_TIDY} --version 2>/dev/null | head -1))"
if [ ! -f "${BUILD_DIR}/compile_commands.json" ]; then
	echo "[lint] no ${BUILD_DIR}/compile_commands.json -- configure with"
	echo "       -DCMAKE_EXPORT_COMPILE_COMMANDS=ON and build first."
	rc=1
elif [ -z "${TIDY_DIFF}" ]; then
	echo "[lint] clang-tidy-diff.py not found (set CLANG_TIDY_DIFF)."
	rc=1
else
	log="$(mktemp)"
	git diff -U0 "${BASE}" -- 'src/*.cpp' 'src/*.hpp' 'tests/*.cpp' 'tests/*.hpp' \
		| python3 "${TIDY_DIFF}" -p1 -path "${BUILD_DIR}" \
			-clang-tidy-binary "${CLANG_TIDY}" \
			-config-file .clang-tidy -only-check-in-db -quiet -j"${CORES}" \
			2>&1 | tee "${log}"
	if grep -qE 'warning:|error:' "${log}"; then
		echo "[lint] clang-tidy: issues found on changed lines"
		rc=1
	fi
	rm -f "${log}"
fi

[ "${rc}" -eq 0 ] && echo "[lint] OK" || echo "[lint] FAILED"
exit "${rc}"
