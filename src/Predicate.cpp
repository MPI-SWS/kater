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

#include "Predicate.hpp"
#include <cassert>
#include <iostream>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

auto operator<<(std::ostream &ostr, const Predicate &p) -> std::ostream &
{
	return ostr << p.getID() << (p.isComplement() ? "-1" : "");
	;
}

auto operator<<(std::ostream &ostr, const PredicateSet &preds) -> std::ostream &
{
	ostr << "[";

	auto first = true;
	for (const auto &p : preds) {
		ostr << (!first ? "&" : "") << p;
		first = false;
	}

	ostr << "]";
	return ostr;
}
