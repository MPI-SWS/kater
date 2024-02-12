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

#include "ParsingDriver.hpp"

#include "Builtins.hpp"

#include <cstring>
#include <fstream>
#include <iostream>

#define DEBUG_TYPE "parser"

extern FILE *yyin;
extern void yyrestart(FILE *);

ParsingDriver::ParsingDriver() : module(new KatModule) { registerBuiltins(*module); }

void ParsingDriver::saveState() { states.emplace_back(getLocation(), yyin, dir, getPrefix()); }

void ParsingDriver::restoreState()
{
	if (states.empty())
		return;

	auto &s = states.back();
	yyrestart(s.in);
	location = s.loc;
	dir = s.dir;
	prefix = s.prefix;
	states.pop_back();
}

auto ParsingDriver::parse(const std::string &name) -> int
{
	saveState();

	if (name.empty()) {
		std::cerr << "no input file provided" << std::endl;
		exit(EXIT_FAILURE);
	}

	auto path = dir + name;
	if ((yyin = fopen(path.c_str(), "r")) == nullptr) {
		std::cerr << "cannot open " << path << ": " << strerror(errno) << std::endl;
		exit(EXIT_FAILURE);
	}

	auto s = path.find_last_of("/");
	dir = path.substr(0, s != std::string::npos ? s + 1 : std::string::npos);

	auto d = path.find_last_of(".");
	prefix = path.substr(s != std::string::npos ? s + 1 : 0,
			     d == std::string::npos ? std::string::npos
						    : (s != std::string::npos ? d - s - 1 : d - 1));

	yyrestart(yyin);
	location.initialize(&path);

	yy::parser parser(*this);

	// KATER_DEBUG(
	// 	if (config.debug)
	// 		parser.set_debug_level(2);
	// );

	auto res = parser.parse();

	fclose(yyin);

	/* If @ top-level, save ppo, hb_stable */
	if (states.size() == 1) {
		module->registerPPO(module->getRegisteredID(getQualifiedName("ppo")));
		module->registerHB(module->getRegisteredID(getQualifiedName("hb_stable")));
	}

	restoreState();

	return res;
}
