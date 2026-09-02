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

#include "DbgInfo.hpp"
#include "VSet.hpp"

#include "Error.hpp"

#include <algorithm>
#include <array>
#include <cassert>
#include <compare>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

class PredicateSet;
class Theory;

using PredExport = std::string;
struct PredicateInfo {
	std::string name;
	PredExport genmc;
	std::optional<DbgInfo> dbg;
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
		ATOM,
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
		PROT,
		HPPROT,
		NOTHPPROT,
		/* Locking */
		LK,
		UL,
		PLK,
		NPLK,
		/* Method calls */
		MB,
		ME,
		/* Others */
		HEAP,
		REC,
		D,
		DEP,
		LOC
	};

	Predicate() = default;

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

	/* One word: sets store these inline, so a Predicate's size decides how much
	 * a sorted transition vector shifts per insert. 31 bits is ample */
	ID id : 31 {};
	bool comp : 1 {false};

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
	/*
	 * We keep the predicates inline. A Transition holds a TransLabel by value,
	 * which holds two of these, so this is what makes a Transition trivially
	 * copyable. Kater uses two or three predicates per set; four is the most we
	 * have seen.
	 */
	static constexpr unsigned inlineCapacity = 8;
	using const_iterator = const Predicate *;

	PredicateSet() = default;
	PredicateSet(Predicate p) : size_(1) { preds_[0] = p; }
	PredicateSet(std::initializer_list<Predicate> preds)
	{
		for (auto p : preds)
			insert(p);
	}

	[[nodiscard]] auto begin() const -> const_iterator { return preds_.data(); }
	[[nodiscard]] auto end() const -> const_iterator { return preds_.data() + size_; }
	[[nodiscard]] auto preds() const { return std::ranges::subrange(begin(), end()); }

	[[nodiscard]] auto empty() const -> bool { return size_ == 0; }
	[[nodiscard]] auto size() const -> unsigned { return size_; }

	auto insert(const Predicate &p) -> bool
	{
		auto *it = std::lower_bound(begin(), end(), p);
		if (it != end() && *it == p)
			return false;
		VERIFY(size_ < inlineCapacity, "too many composing predicates on a transition");
		auto pos = it - begin();
		for (auto i = size_; i > pos; i--)
			preds_[i] = preds_[i - 1];
		preds_[pos] = p;
		size_++;
		return true;
	}
	auto insert(const PredicateSet &other) -> bool
	{
		auto result = false;
		for (const auto &p : other)
			result |= insert(p);
		return result;
	}

	[[nodiscard]] auto contains(const Predicate &p) const -> bool
	{
		return std::binary_search(begin(), end(), p);
	}
	[[nodiscard]] auto contains(const PredicateSet &other) const -> bool
	{
		return std::includes(begin(), end(), other.begin(), other.end());
	}

	void minus(const Predicate &p)
	{
		auto *it = std::lower_bound(begin(), end(), p);
		if (it == end() || !(*it == p))
			return;
		auto pos = it - begin();
		for (auto i = pos; i + 1 < size_; i++)
			preds_[i] = preds_[i + 1];
		size_--;
	}
	void minus(const PredicateSet &other)
	{
		for (const auto &p : other)
			minus(p);
	}

	auto operator<=>(const PredicateSet &other) const
	{
		return std::lexicographical_compare_three_way(begin(), end(), other.begin(),
							      other.end());
	}
	auto operator==(const PredicateSet &other) const -> bool
	{
		return size_ == other.size_ && std::equal(begin(), end(), other.begin());
	}

	/** Prints the conjunction as [a&b]; resolves names if THEORY is given */
	auto dump(std::ostream &ostr, const Theory *theory = nullptr) const -> std::ostream &;

private:
	std::array<Predicate, inlineCapacity> preds_{};
	unsigned size_ = 0;
};

/* Renders PRED via THEORY, or as a debug token if THEORY does not know it */
auto nameOf(const Predicate &pred, const Theory *theory) -> std::string;

#endif /* KATER_PREDICATE_HPP */
