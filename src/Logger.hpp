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

#ifndef KATER_LOGGER_HPP
#define KATER_LOGGER_HPP

#include "DbgInfo.hpp"

#include <cstdint>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>

/* Diagnostic severities, in decreasing order of importance */
enum class VerbosityLevel : std::uint8_t { Quiet, Error, Warning, Note };

/* Renders the severity the way compilers do: "error: ", "warning: ", "note: " */
auto operator<<(std::ostream &ostr, VerbosityLevel lvl) -> std::ostream &;

/* Diagnostics at least as important as this are emitted */
extern VerbosityLevel logLevel; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

/* Returns whether ID has been logged before, recording it if not */
auto alreadyLogged(const std::string &id) -> bool;

/* Starts a continuation line below the current diagnostic */
auto note(std::ostream &ostr) -> std::ostream &;

/*
 * Accumulates one diagnostic and emits it with a single write, so that
 * concurrent diagnostics cannot interleave.
 *
 * Adapted from GenMC's Support/Logger.hpp. It differs in that there is no stdout
 * channel (stdout carries generated code), that logging once is a free function
 * rather than a LoggerOnce subclass, and that a diagnostic can carry a location.
 */
class Logger {

public:
	Logger(VerbosityLevel lvl, const std::optional<DbgInfo> &dbg = {})
	{
		if (dbg.has_value())
			buffer_ << *dbg << ": " << lvl;
		else
			buffer_ << "kater: " << lvl;
	}

	Logger(const Logger &) = delete;
	Logger(Logger &&) = delete;
	auto operator=(const Logger &) -> Logger & = delete;
	auto operator=(Logger &&) -> Logger & = delete;

	template <typename T> auto operator<<(const T &msg) -> Logger &
	{
		buffer_ << msg;
		return *this;
	}

	~Logger();

private:
	std::ostringstream buffer_;
};

// NOLINTBEGIN(cppcoreguidelines-macro-usage)
#define LOG(level)                                                                                 \
	if ((level) > logLevel)                                                                    \
		;                                                                                  \
	else                                                                                       \
		Logger(level)

#define LOG_AT(level, dbg)                                                                         \
	if ((level) > logLevel)                                                                    \
		;                                                                                  \
	else                                                                                       \
		Logger(level, dbg)

#define LOG_ONCE(id, level)                                                                        \
	if ((level) > logLevel || alreadyLogged(id))                                               \
		;                                                                                  \
	else                                                                                       \
		Logger(level)

#define LOG_ONCE_AT(id, level, dbg)                                                                \
	if ((level) > logLevel || alreadyLogged(id))                                               \
		;                                                                                  \
	else                                                                                       \
		Logger(level, dbg)
// NOLINTEND(cppcoreguidelines-macro-usage)

#endif /* KATER_LOGGER_HPP */
