#include "Error.hpp"
#include <string>
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

void addDebugType(const char *dbgType)
{
	currentDbgTypes.push_back(dbgType);
}

#endif /* ENABLE_KATER_DEBUG */
