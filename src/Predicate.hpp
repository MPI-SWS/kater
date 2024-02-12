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

#include "VSet.hpp"

#include <algorithm>
#include <cassert>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

class PredicateSet;
using PredExport = std::string;
struct PredicateInfo {
	std::string name;
	PredExport genmc;
};

/**
 * Represents a predicate (e.g., R,W,F, etc). Each predicate
 * has a unique (natural) identifier, with negative numbers
 * being reserved for user-defined predicates.
 */
class Predicate {

public:
	using ID = int;

	/** Builtin predicates  */
	enum class BuiltinID {
		/* Access modes */
		NA = 0,
		RLX,
		ACQ,
		REL,
		SC,
		/* Memory accesses */
		W,
		R,
		/* Exclusivity flags */
		EXCL,
		NEXCL,
		/* Fences */
		F,
		/* Thread events */
		TC,
		TJ,
		TB,
		TE,
		TK,
		/* Allocation */
		ALLOC,
		FREE,
		HPRET,
		HPPROT,
		NOTHPPROT,
		/* Locking */
		LK,
		UL,
		/* Others */
		HEAP,
		REC,
		D,
		DEP,
		LOC
	};

	Predicate() = delete;

	/** Creates a built-in predicate */
	static auto createBuiltin(BuiltinID b) -> Predicate { return {getBuiltinID(b)}; }

	/** Creates a fresh, user predicate */
	static auto createUser() -> Predicate { return {getFreshID()}; }

	/** Returns the ID of the predicate */
	[[nodiscard]] constexpr auto getID() const -> ID { return id; }

	/** Whether the predicate is builtin or not */
	[[nodiscard]] constexpr auto isBuiltin() const -> bool { return id >= 0; }

	/** Complements this predicate */
	auto complement() -> Predicate &
	{
		comp = !comp;
		return *this;
	}

	/* Whether this relation is inversed */
	[[nodiscard]] constexpr auto isComplement() const -> bool { return comp; }

	auto operator<=>(const Predicate &) const = default;
	friend auto operator<<(std::ostream &ostr, const Predicate &p) -> std::ostream &;

private:
	friend class PredicateSet;

	Predicate(ID id) : id(id) {}

	/** Returns the unique identifier of the given builtin */
	static auto getBuiltinID(BuiltinID b) -> ID
	{
		return static_cast<std::underlying_type_t<BuiltinID>>(b);
	}

	/** Returns a fresh unique identifier */
	static auto getFreshID() -> ID { return --dispenser; }

	ID id;
	bool comp = false;

	static inline ID dispenser{}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
};

struct PredicateHasher {
	auto operator()(const Predicate &p) const -> size_t
	{
		return std::hash<Predicate::ID>()(p.getID());
	}
};

/*
 * A collection of predicates that compose, i.e., their intersection
 * is non-empty.
 */
class PredicateSet {

public:
	using PredSet = VSet<Predicate>;

	// using iterator = PredSet::iterator;
	using const_iterator = PredSet::const_iterator;

	PredicateSet() = default;
	PredicateSet(Predicate p) : preds_({p}) {}
	PredicateSet(std::initializer_list<Predicate> preds) : preds_(preds) {}

	// auto begin() -> iterator { return preds_.begin(); }
	// auto end() -> iterator { return preds_.end(); }
	// auto preds() { return std::ranges::ref_view(preds_); }

	[[nodiscard]] auto begin() const -> const_iterator { return preds_.begin(); }
	[[nodiscard]] auto end() const -> const_iterator { return preds_.end(); }
	[[nodiscard]] auto preds() const { return std::ranges::ref_view(preds_); }

	/** Whether the predicate set is empty */
	[[nodiscard]] auto empty() const -> bool { return preds_.empty(); }

	/** Returns the size of the predicate set */
	[[nodiscard]] auto size() const { return preds_.size(); }

	/* Inserts P into THIS (if THIS does not already contain P).
	 * Returns whether the insertion took place */
	auto insert(const Predicate &p) -> bool { return preds_.insert(p).second; }
	auto insert(const PredicateSet &other) -> bool
	{
		auto result = false;
		for (const auto &p : other.preds_)
			result |= preds_.insert(p).second;
		return result;
	}

	/* Whether THIS contains element P */
	[[nodiscard]] auto contains(const Predicate &p) const -> bool { return preds_.contains(p); }

	/* Whether THIS contains set OTHER */
	[[nodiscard]] auto contains(const PredicateSet &other) const -> bool
	{
		return other.preds_.subsetOf(this->preds_);
	}

	/** Removes P from THIS */
	void minus(const Predicate &p) { preds_.erase(p); }

	/** Removes all predicates appearing in OTHER from THIS */
	void minus(const PredicateSet &other)
	{
		for (const auto &p : other.preds())
			preds_.erase(p);
	}

	inline auto operator<=>(const PredicateSet &other) const = default;

	friend auto operator<<(std::ostream &ostr, const PredicateSet &preds) -> std::ostream &;

private:
	PredSet preds_{};
};

#endif /* KATER_PREDICATE_HPP */
