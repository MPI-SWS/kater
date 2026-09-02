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

#include "Counterexample.hpp"

#include <ostream>

auto Counterexample::dump(std::ostream &ostr, const Theory *theory) const -> std::ostream &
{
	if (empty())
		return ostr << "ε";

	auto idx = 0U;
	for (const auto &lab : *this) {
		lab.dump(ostr, theory) << " ";
		if (getType() == Type::TUT && idx++ == getMismatch())
			ostr << "===> ";
	}
	if (getType() == Type::ANA)
		ostr << "(A/NA)";
	return ostr;
}
