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

#include "Printer.hpp"

#include "Config.hpp"
#include "Error.hpp"
#include "KatModule.hpp"
#include "NFAUtils.hpp"
#include "Utils.hpp"

#include <fstream>
#include <sstream>

Printer::Printer(const KatModule &module, const Config &config) : module_(module), config_(config)
{
	/* Construct all the names to be used */
	std::transform(getConf().name.begin(), getConf().name.end(), std::back_inserter(prefix_),
		       ::toupper);
}
