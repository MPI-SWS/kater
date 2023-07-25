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

#include "Predicate.hpp"
#include <unordered_map>

/*******************************************************************************
 **                           RelationInfo Class
 ******************************************************************************/

enum class RelType  { OneOne, ManyOne, UnsuppMany, Conj, Final };

struct RelationInfo {
	std::string  name;
	RelType      type;
	PredicateSet dom;
	PredicateSet codom;
	bool         insidePo;
};


/*******************************************************************************
 **                           Relation Class
 ******************************************************************************/

class Relation {

public:
	/* Negative IDs reserved for user-defined relations. */
	using ID = int;

        enum Builtin {
		/*** CAUTION: The dummy IDs should not be a part of the builtin map */

		/* same thread */
		same_thread,
		/* same location */
		alloc,
		frees,
		loc_overlap,
		/* tc, tj */
		tc,
		tj,
		/* rf, co, fr */
		PerLocBegin,
		rf,
		mo_imm,
		fr_imm,
		/* rfe, coe, fre */
		rfe,
		moe,
		fre,
		/* deps */
		WithinPoBegin,
		ctrl_imm,
		addr_imm,
		data_imm,
		/* rfi, coi, fri*/
		rfi,
		moi,
		fri,
		/* detour */
		detour,
		/* po-loc */
		po_loc_imm,
		WithinPoLast,
		PerLocLast,
		/* po */
		po_imm,
		/* any */
		any,
	};

	using builtin_iterator = std::unordered_map<Relation::Builtin,
						    RelationInfo>::iterator;
	using builtin_const_iterator = std::unordered_map<Relation::Builtin,
							  RelationInfo>::const_iterator;

protected:
	Relation() = delete;
	Relation(ID id, bool inverse = false) : id(id), inverse(inverse) {}

public:
	static auto createBuiltin(Builtin b) -> Relation { return {getBuiltinID(b)}; }
	static auto createUser() -> Relation { return {getFreshID()}; }

	static auto getBuiltinID(Builtin b) -> ID { return b; }

	static auto builtin_begin() -> builtin_const_iterator { return builtins.begin(); }
	static auto builtin_end() -> builtin_const_iterator { return builtins.end(); }

	[[nodiscard]] constexpr auto getID() const -> ID { return id; }

	[[nodiscard]] constexpr auto isBuiltin() const -> bool { return getID() >= 0; }

	[[nodiscard]] constexpr auto toBuiltin() const -> Builtin {
		assert(isBuiltin());
		return static_cast<Builtin>(getID());
	}

	void markAsPerLocOf(const Relation &other) const {
		perlocs.insert({other.getID(), *this});
	}

	[[nodiscard]] auto getPerLoc() const -> Relation {
		assert(perlocs.count(this->getID()));
		return perlocs.find(this->getID())->second;
	}

	[[nodiscard]] auto hasPerLoc() const -> bool { return perlocs.count(this->getID()) != 0U; }

	/* Inverses this relation */
	void invert() { inverse = !inverse; }

	/* Whether this relation is inversed */
	[[nodiscard]] constexpr auto isInverse() const -> bool { return inverse; }

	/* ***builtins only*** returns the domain of the relation */
	[[nodiscard]] auto getDomain() const -> const PredicateSet &;

	/* ***builtins only*** returns the codomain of the relation */
	[[nodiscard]] auto getCodomain() const -> const PredicateSet &;

	/* ***builtins only*** returns the type of the relation */
	[[nodiscard]] auto getType() const -> RelType {
		assert(isBuiltin());
		return builtins.find(toBuiltin())->second.type;
	}

	/* Returns true if OTHER is included in THIS */
	[[nodiscard]] auto includes(const Relation &other) const -> bool;

	[[nodiscard]] auto getName() const -> std::string;

	auto operator==(const Relation &other) const -> bool {
		return getID() == other.getID() && isInverse() == other.isInverse();
	}
	auto operator!=(const Relation &other) const -> bool {
		return !(*this == other);
	}

	auto operator<(const Relation &other) const -> bool {
		return getID() < other.getID() ||
		       (getID() == other.getID() && static_cast<int>(isInverse()) < static_cast<int>(other.isInverse()));
	}
	auto operator>=(const Relation &other) const -> bool {
		return !(*this < other);
	}

	auto operator>(const Relation &other) const -> bool {
		return getID() > other.getID() ||
		       (getID() == other.getID() && static_cast<int>(isInverse()) > static_cast<int>(other.isInverse()));
	}
	auto operator<=(const Relation &other) const -> bool {
		return !(*this > other);
	}

private:
	static auto getFreshID() -> ID { return --dispenser; }

	ID id;
	bool inverse = false;

	static inline ID dispenser = 0;
	static const std::unordered_map<Relation::Builtin, RelationInfo> builtins;
	static std::unordered_map<Relation::ID, Relation> perlocs;
};

struct RelationHasher {
    auto operator()(const Relation &r) const -> size_t {
	    return std::hash<Relation::ID>()(r.getID());
    }
};

#endif /* KATER_RELATION_HPP */
