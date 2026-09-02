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

#include "TransLabel.hpp"

#include "Predicate.hpp"
#include "Relation.hpp"
#include "Theory.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <set>
#include <unordered_map>
#include <vector>

auto TransLabel::merge(const TransLabel &other,
		       const std::function<bool(const TransLabel &)> &isValid) -> bool
{
	if (!isValid(*this) || !isValid(other)) {
		return false;
	}
	if (!isPredicate() && !other.isPredicate()) {
		return false;
	}

	if (isPredicate()) {
		/* Do not merge into THIS before ensuring combo is valid */
		TransLabel t(*this);
		t.getRelation() = other.getRelation();
		t.getPreChecks().insert(other.getPreChecks());
		assert(t.getPostChecks().empty());
		t.getPostChecks() = other.getPostChecks();
		if (isValid(t)) {
			*this = t;
		}
		return isValid(t);
	}

	assert(other.isPredicate());

	TransLabel t(*this);
	t.getPostChecks().insert(other.getPreChecks());
	if (isValid(t)) {
		*this = t;
	}
	return isValid(t);
}

auto TransLabel::dump(std::ostream &ostr, const Theory *theory) const -> std::ostream &
{
	if (isPredicate()) {
		return getPreChecks().dump(ostr, theory);
	}

	if (!getPreChecks().empty()) {
		getPreChecks().dump(ostr, theory) << ";";
	}
	ostr << nameOf(*getRelation(), theory);
	if (!getPostChecks().empty()) {
		getPostChecks().dump(ostr << ";", theory);
	}
	return ostr;
}
