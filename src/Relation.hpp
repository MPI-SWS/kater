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

#ifndef KATER_RELATION_HPP
#define KATER_RELATION_HPP

#include "DbgInfo.hpp"
#include "Predicate.hpp"

#include <optional>
#include <string>
#include <unordered_map>

/*******************************************************************************
 **                           RelationInfo Class
 ******************************************************************************/

enum class RelArity { Unknown, OneOne, ManyOne, OneMany, UnsuppMany, Conj, Final };
enum class RelLocInfo { Unknown, ChangesLoc, KeepsLoc };
struct RelExport {
	std::string succ;
	std::string pred;
};

struct RelationInfo {
	std::string name{};
	RelArity arity{};
	RelLocInfo locInfo{};
	PredicateSet dom{};
	PredicateSet codom{};
	bool hidden{};
	RelExport genmc{};
	std::optional<DbgInfo> dbg{};
};

/*******************************************************************************
 **                           Relation Class
 ******************************************************************************/

class Relation {

public:
	/* Negative IDs reserved for user-defined relations. */
	using ID = int;

	enum BuiltinID {
		/*** CAUTION: Dummy IDs (e.g., PerLocBegin) should not be a part of the builtin map
		 */

		/* same thread */
		same_thread,
		/* same location */
		alloc,
		frees,
		loc_overlap,
		/* tc, tj */
		tc,
		tj,
		/* rf, co, fr, rmw */
		PerLocBegin,
		rf,
		mo_imm,
		mo,
		fr_imm,
		fr,
		/* rfe, coe, fre */
		rfe,
		moe,
		fre,
		/* deps */
		WithinPoBegin,
		ctrl,
		addr,
		data,
		/* rfi, coi, fri*/
		rfi,
		moi,
		fri,
		/* detour */
		detour,
		/* rmw, po-loc */
		rmw,
		po_loc_imm,
		po_loc,
		PerLocLast,
		po_imm,
		WithinPoLast,
		/* po */
		po,
		/* any */
		any,
	};

	Relation() = delete;

	static auto createBuiltin(BuiltinID builtin) -> Relation { return {builtin}; }
	static auto createUser() -> Relation { return {getFreshID()}; }

	[[nodiscard]] constexpr auto getID() const -> ID { return id; }

	[[nodiscard]] constexpr auto isBuiltin() const -> bool { return getID() >= 0; }
	[[nodiscard]] constexpr auto isUser() const -> bool { return !isBuiltin(); }

	[[nodiscard]] constexpr auto toBuiltin() const -> BuiltinID
	{
		assert(isBuiltin());
		return static_cast<BuiltinID>(getID());
	}

	/* Inverses this relation */
	auto invert() -> Relation &
	{
		inverse = !inverse;
		return *this;
	}

	/* Whether this relation is inversed */
	[[nodiscard]] constexpr auto isInverse() const -> bool { return inverse; }

	[[nodiscard]] auto getName() const -> std::string;

	auto operator<=>(const Relation &) const = default;
	friend auto operator<<(std::ostream &ostr, const Relation &r) -> std::ostream &;

private:
	Relation(ID id) : id(id) {}

	static auto getFreshID() -> ID { return --dispenser; }

	ID id{};
	bool inverse = false;

	static inline ID dispenser{}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
};

struct RelationHasher {
	auto operator()(const Relation &r) const -> size_t
	{
		return std::hash<Relation::ID>()(r.getID());
	}
};

#endif /* KATER_RELATION_HPP */
