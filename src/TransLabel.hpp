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

#ifndef KATER_TRANS_LABEL_HPP
#define KATER_TRANS_LABEL_HPP

#include "Predicate.hpp"
#include "Relation.hpp"
#include <cassert>
#include <functional>
#include <iostream>
#include <optional>
#include <set>
#include <vector>

/*******************************************************************************
 *                           TransLabel class
 ******************************************************************************/

/*
 * Represents the label of an NFA transition
 */
class TransLabel {

public:
	/* Costructors/destructor */
	TransLabel() = delete;
	TransLabel(std::optional<Relation> id, const PredicateSet &preG = {},
		   const PredicateSet &postG = {})
		: id(id), preChecks(preG), postChecks(postG) {}

	[[nodiscard]] auto getRelation() const -> const std::optional<Relation> & { return id; }
	auto getRelation() -> std::optional<Relation> & { return id; }

	[[nodiscard]] auto getPreChecks() const -> const PredicateSet & { return preChecks; }
	auto getPreChecks() -> PredicateSet & { return preChecks; }

	[[nodiscard]] auto getPostChecks() const -> const PredicateSet & { return postChecks; }
	auto getPostChecks() -> PredicateSet & { return postChecks; }

	[[nodiscard]] auto isPredicate() const -> bool { return !getRelation(); }

	[[nodiscard]] auto isRelation() const -> bool { return !isPredicate(); }

	[[nodiscard]] auto hasPreChecks() const -> bool { return !getPreChecks().empty(); }

	[[nodiscard]] auto hasPostChecks() const -> bool { return !getPostChecks().empty(); }

	[[nodiscard]] auto isBuiltin() const -> bool { return !getRelation().has_value() || getRelation()->isBuiltin(); }

	[[nodiscard]] auto getCalcIndex() const -> int { assert(!isBuiltin()); return -(getRelation()->getID() + 1); }

	void flip() {
		/* Do not flip ε transitions; maintain unique representation */
		if (isPredicate()) {
			assert(!hasPostChecks());
			return;
		}
		getRelation()->invert();
		std::swap(getPreChecks(), getPostChecks());
	}

	/* Attemps to merge OTHER into THIS and returns whether it
	 * succeeded.  Two transitions can be merged if at least one
	 * of them is an epsilon transition */
	auto merge(const TransLabel &other,
		   const std::function<bool(const TransLabel &)>& isValid = [](auto & /*lab*/){ return true; }) -> bool;

	[[nodiscard]] auto composesWith(const TransLabel &other) const -> bool;

	[[nodiscard]] auto matches(const TransLabel &other) const -> bool {
		return getPreChecks().includes(other.getPreChecks()) &&
			(!isRelation() || (other.isRelation() && getRelation()->includes(*other.getRelation()))) &&
			getPostChecks().includes(other.getPostChecks());
	}

	[[nodiscard]] auto toString() const -> std::string;

	auto operator==(const TransLabel &other) const -> bool {
		return getRelation() == other.getRelation() &&
			getPreChecks() == other.getPreChecks() &&
			getPostChecks() == other.getPostChecks();
	}
	auto operator!=(const TransLabel &other) const -> bool {
		return !operator==(other);
	}

	auto operator<(const TransLabel &other) const -> bool {
		return getRelation() < other.getRelation() ||
			(getRelation() == other.getRelation() &&
				(getPreChecks() < other.getPreChecks() ||
				 (getPreChecks() == other.getPreChecks() &&
				  getPostChecks() < other.getPostChecks())));
	}
	auto operator<=(const TransLabel &other) const -> bool {
		return operator==(other) || operator<(other);
	}
	auto operator>=(const TransLabel &other) const -> bool {
		return !operator<(other);
	}
	auto operator>(const TransLabel &other) const -> bool {
		return !operator<(other) && !operator==(other);
	}

	friend auto operator<<(std::ostream &s, const TransLabel &t) -> std::ostream & {
		return s << t.toString();
	}

private:
	std::optional<Relation> id;
	static int calcNum;

	PredicateSet preChecks;
	PredicateSet postChecks;
};

#endif /* KATER_TRANS_LABEL_HPP */
