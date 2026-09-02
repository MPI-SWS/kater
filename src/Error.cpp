#include "Error.hpp"

#include <cctype>
#include <cstdlib>
#include <format>
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>

#ifdef ENABLE_KATER_DEBUG

bool katerDebug = false;

static std::vector<std::string> currentDbgTypes;

bool isCurrentDebugType(const char *dbgType)
{
	if (currentDbgTypes.empty())
		return true;

	/* Do not use find() so that we don't create a string */
	for (auto &d : currentDbgTypes) {
		if (d == dbgType)
			return true;
	}
	return false;
}

void addDebugType(const char *dbgType) { currentDbgTypes.push_back(dbgType); }

#endif /* ENABLE_KATER_DEBUG */

namespace kater::detail {

#ifdef ENABLE_KATER_DEBUG
/* Let the user attach a debugger, as genmc does; useless without a terminal */
static void debuggableExit()
{
	if (isatty(STDIN_FILENO) == 0)
		std::abort();

	while (true) {
		std::cerr << "\n(A)bort, (S)top/Trap, (G)DB\n> " << std::flush;
		char answer = 0;
		if (!(std::cin >> answer))
			std::abort();

		switch (std::toupper(answer)) {
		case 'A':
			std::abort();
		case 'S':
			__builtin_trap();
			return;
		case 'G': {
			const auto cmd = std::format("gdb -p {}", getpid());
			std::cerr << "Executing: " << cmd << " ...\n";
			if (std::system(cmd.c_str()) != 0) // NOLINT(concurrency-mt-unsafe)
				std::cerr << "Failed to start gdb.\n";
			return;
		}
		default:
			std::cerr << "Invalid option.\n";
		}
	}
}
#endif

void reportInternalError(const char *what, const char *file, unsigned line)
{
	std::cerr << file << ":" << line << ": kater: " << what << "\n"
		  << "This is a bug in kater; please report it.\n";
#ifdef ENABLE_KATER_DEBUG
	debuggableExit();
#endif
	std::abort();
}

} // namespace kater::detail
