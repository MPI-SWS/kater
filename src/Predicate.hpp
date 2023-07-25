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

#ifndef KATER_PREDICATE_HPP
#define KATER_PREDICATE_HPP

#include <algorithm>
#include <cassert>
#include <string>
#include <utility>
#include <vector>

/*******************************************************************************
 **                           PredicateMask
 ******************************************************************************/

enum PredicateMask : unsigned long long;

/*******************************************************************************
 **                           PredicateInfo
 ******************************************************************************/

struct PredicateInfo {
	std::string name;
	std::string genmcString;
};

/*******************************************************************************
 **                           PredicateSet Class
 ******************************************************************************/

/*
 * A collection of predicates that compose, i.e., their intersection
 * is non-empty.
 */
class PredicateSet {

public:

	using builtin_iterator = std::vector<std::pair<PredicateMask, PredicateInfo>>::const_iterator;

	PredicateSet() = default;
	PredicateSet(PredicateMask p) : mask(p) {}

	/* Whether the collection is empty */
	[[nodiscard]] auto empty() const -> bool;

	/* Returns true if the combination of two sets is valid */
	[[nodiscard]] auto composes(const PredicateSet &other) const -> bool;

	/* Returns true if OTHER is included in THIS */
	[[nodiscard]] auto includes(const PredicateSet &other) const -> bool;

	/* Tries to merge OTHER into THIS (if the combo is valid).
	 * Returns whether the merge was successful */
	auto merge(const PredicateSet &other) -> bool;

	/* Removes all predicates appearing in OTHER from THIS */
	void minus(const PredicateSet &other);

	auto operator==(const PredicateSet &other) const -> bool {
		return mask == other.mask;
	}
	auto operator!=(const PredicateSet &other) const -> bool {
		return !(*this == other);
	}

	auto operator<(const PredicateSet &other) const -> bool {
		return mask < other.mask;
	}
	auto operator>=(const PredicateSet &other) const -> bool {
		return !(*this < other);
	}

	auto operator>(const PredicateSet &other) const -> bool {
		return mask > other.mask;
	}
	auto operator<=(const PredicateSet &other) const -> bool {
		return !(*this > other);
	}

	static const std::vector<std::pair<PredicateMask, PredicateInfo>> builtins;
	static auto builtin_begin() -> builtin_iterator { return builtins.begin(); }
	static auto builtin_end() -> builtin_iterator { return builtins.end(); }

	[[nodiscard]] auto toGenmcString() const -> std::string;
	
	friend auto operator<<(std::ostream& ostr, const PredicateSet &s) -> std::ostream &;

private:
	PredicateMask mask;
};

#endif /* KATER_PREDICATE_HPP */
