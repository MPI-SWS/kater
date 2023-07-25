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

#ifndef KATER_KAT_MODULE_API_HPP
#define KATER_KAT_MODULE_API_HPP

#include "RegExp.hpp"

using URE = std::unique_ptr<RegExp>;
using UCO = std::unique_ptr<Constraint>;

enum class VarStatus { Normal, Reduce, View };

// FIXME: Polymorphism
struct SavedVar {
	SavedVar(URE exp) : exp(std::move(exp)) {}
	SavedVar(URE exp, NFA::ReductionType t, URE red)
		: exp(std::move(exp)), status(VarStatus::Reduce),
		  redT(t), red(std::move(red)) {}

	SavedVar(URE exp, VarStatus s)
		: exp(std::move(exp)), status(s) {}

	URE exp = nullptr;
	VarStatus status = VarStatus::Normal;
	NFA::ReductionType redT = NFA::ReductionType::Self;
	URE red = nullptr;
};

#endif /* KATER_KAT_MODULE_API_HPP */
