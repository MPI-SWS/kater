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

#ifndef KATER_INCLUSION_HPP
#define KATER_INCLUSION_HPP

#include <string>
#include <utility>

#include "Constraint.hpp"

template <typename T> struct Inclusion {
	Inclusion(T &&lhs, T &&rhs, Constraint::Type t, std::string s)
		: lhs(std::move(lhs)), rhs(std::move(rhs)), type(t), s(std::move(s))
	{
	}

	T lhs;
	T rhs;
	Constraint::Type type;
	std::string s;
};

#endif /* KATER_INCLUSION_HPP */
