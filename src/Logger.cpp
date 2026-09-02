/*
 * KATER -- Automating Weak Memory Model Metatheory
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can access it online at
 * http://www.gnu.org/licenses/gpl-3.0.html.
 */

#include "Logger.hpp"

#include <iostream>
#include <mutex>
#include <set>
#include <string>

VerbosityLevel logLevel = // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
	VerbosityLevel::Note;

auto operator<<(std::ostream &ostr, VerbosityLevel lvl) -> std::ostream &
{
	switch (lvl) {
	case VerbosityLevel::Error:
		return ostr << "error: ";
	case VerbosityLevel::Warning:
		return ostr << "warning: ";
	case VerbosityLevel::Note:
		return ostr << "note: ";
	case VerbosityLevel::Quiet:
		break;
	}
	return ostr;
}

auto alreadyLogged(const std::string &id) -> bool
{
	static std::mutex mutex;
	static std::set<std::string> seen;

	const std::scoped_lock lock(mutex);
	return !seen.insert(id).second;
}

auto note(std::ostream &ostr) -> std::ostream & { return ostr << "\n  note: "; }

Logger::~Logger()
{
	/* Emit in one write, so that concurrent diagnostics cannot interleave */
	buffer_ << "\n";
	std::cerr << buffer_.str();
}
