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

#ifndef KATER_REGEXP_HPP
#define KATER_REGEXP_HPP

#include "NFA.hpp"
#include "StatePair.hpp"
#include "TransLabel.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <typeinfo>
#include <unordered_map>
#include <utility>

/*******************************************************************************
 **                           RegExp Class (Abstract)
 ******************************************************************************/

class AltRE;
class Theory;

class RegExp {

protected:
	using RecStatesMap = std::unordered_map<Relation, NFA::State *, RelationHasher>;
	RegExp(std::vector<std::unique_ptr<RegExp>> &&kids = {}) : kids_(std::move(kids)) {}

public:
	virtual ~RegExp() = default;

	RegExp(const RegExp &) = delete;
	RegExp(RegExp &&) = default;

	static auto createFalse() -> std::unique_ptr<RegExp>;
	static auto createId() -> std::unique_ptr<RegExp>;

	static auto createSym(std::unique_ptr<RegExp> re) -> std::unique_ptr<RegExp>;

	auto kid_begin() { return getKids().begin(); }
	auto kid_end() { return getKids().end(); }
	auto kids() { return std::ranges::ref_view(getKids()); }

	[[nodiscard]] auto kid_begin() const { return getKids().begin(); }
	[[nodiscard]] auto kid_end() const { return getKids().end(); }
	[[nodiscard]] auto kids() const { return std::ranges::ref_view(getKids()); }

	/* Fetches the i-th kid */
	[[nodiscard]] auto getKid(unsigned i) const -> const RegExp *
	{
		assert(i < getNumKids() && "Index out of bounds!");
		return kids_[i].get();
	}
	auto getKid(unsigned i) -> std::unique_ptr<RegExp> &
	{
		assert(i < getNumKids() && "Index out of bounds!");
		return kids_[i];
	}

	/* Sets the i-th kid to e */
	void setKid(unsigned i, std::unique_ptr<RegExp> e)
	{
		assert(i < getNumKids() && "Index out of bounds!");
		kids_[i] = std::move(e);
	}

	/* Releases ownership of i-th kid */
	auto releaseKid(unsigned i) -> std::unique_ptr<RegExp>
	{
		assert(i < getNumKids() && "Index out of bounds!");
		return std::move(kids_[i]);
	}

	/* Returns whether this RE has kids */
	[[nodiscard]] auto hasKids() const -> bool { return !kids_.empty(); }

	/* The kids of this RE */
	[[nodiscard]] auto getNumKids() const -> size_t { return kids_.size(); }

	[[nodiscard]] auto isFalse() const -> bool;

	[[nodiscard]] auto isPredicate() const -> bool;
	[[nodiscard]] auto isRelation() const -> bool;

	[[nodiscard]] auto isAnyRelation() const -> bool;

	[[nodiscard]] auto getDomain() const -> std::unique_ptr<RegExp>;
	[[nodiscard]] auto getCodomain() const -> std::unique_ptr<RegExp>;

	virtual auto flip() -> RegExp &
	{
		for (auto &r : getKids()) {
			r->flip();
		}
		return *this;
	}

	/* Convert the RE to an NFA */
	[[nodiscard]] auto toNFA() const -> NFA;

	/* Helper for toNFA() fastpath --- not supposed to be called directly */
	virtual auto toNFAFast() const -> NFA = 0;

	/* Helper for toNFA() --- not supposed to be called directly */
	virtual void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const = 0;

	/* Returns a clone of the RE */
	[[nodiscard]] virtual auto clone() const -> std::unique_ptr<RegExp> = 0;

	auto operator==(const RegExp &other) const -> bool
	{
		return typeid(*this) == typeid(other) && isEqual(other);
	}
	auto operator!=(const RegExp &other) const -> bool { return !(*this == other); }

	auto operator=(const RegExp &) -> RegExp & = delete;
	auto operator=(RegExp &&) -> RegExp & = default;

	/* Dumps the RE */
	virtual auto dump(std::ostream &s, const Theory *theory = nullptr) const
		-> std::ostream & = 0;

protected:
	[[nodiscard]] virtual auto isEqual(const RegExp &other) const -> bool = 0;

	using KidsC = std::vector<std::unique_ptr<RegExp>>;

	[[nodiscard]] auto getKids() const -> const KidsC & { return kids_; }
	auto getKids() -> KidsC & { return kids_; }

	void addKid(std::unique_ptr<RegExp> k) { kids_.push_back(std::move(k)); }

	KidsC kids_;
};

inline auto operator<<(std::ostream &s, const RegExp &re) -> std::ostream & { return re.dump(s); }

/*******************************************************************************
 **                               Singleton
 ******************************************************************************/

class CharRE : public RegExp {

protected:
	CharRE(TransLabel l) : lab(std::move(l)) {}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<CharRE>
	{
		return std::unique_ptr<CharRE>(new CharRE(std::forward<Ts>(params)...));
	}

	/* Returns the transition label */
	[[nodiscard]] auto getLabel() const -> const TransLabel & { return lab; }
	auto getLabel() -> TransLabel & { return lab; }

	/* Sets the transition label */
	void setLabel(const TransLabel &l) { lab = l; }

	auto flip() -> RegExp & override
	{
		lab.flip();
		return *this;
	}

	[[nodiscard]] auto toNFAFast() const -> NFA override { return {getLabel()}; }

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override
	{
		if (lab.isRelation() && recStates.contains(*lab.getRelation())) {
			nfa.addEpsilonTransition(recStates[*lab.getRelation()], p.second);
		} else {
			nfa.addTransition(p.first, NFA::Transition(lab, p.second));
		}
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getLabel());
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override;

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		return getLabel() == dynamic_cast<const CharRE &>(other).getLabel();
	}

private:
	TransLabel lab;
};

/*
 * RE_1 | RE_2
 */
class AltRE : public RegExp {

protected:
	template <typename... Ts> AltRE(Ts &&...args) : RegExp() { (addKid(std::move(args)), ...); }
	AltRE(std::vector<std::unique_ptr<RegExp>> &&kids = {}) : RegExp(std::move(kids)) {}
	AltRE(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
	{
		addKid(std::move(r1));
		addKid(std::move(r2));
	}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<AltRE>
	{
		return std::unique_ptr<AltRE>(new AltRE(std::forward<Ts>(params)...));
	}

	template <typename... Ts> static auto createOpt(Ts &&...params) -> std::unique_ptr<RegExp>;

	[[nodiscard]] auto toNFAFast() const -> NFA override
	{
		NFA nfa;
		for (const auto &k : getKids()) {
			nfa.alt(std::move(k->toNFAFast()));
		}
		return nfa;
	}

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override
	{
		for (const auto &k : getKids()) {
			k->expandOnNFA(nfa, p, recStates);
		}
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		std::vector<std::unique_ptr<RegExp>> nk;
		for (const auto &k : getKids()) {
			nk.push_back(std::move(k->clone()));
		}
		return create(std::move(nk));
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override
	{
		if (getNumKids() == 0) {
			return s << "0";
		}
		s << "(";
		getKid(0)->dump(s, theory);
		for (int i = 1; i < getNumKids(); i++) {
			s << " | ";
			getKid(i)->dump(s, theory);
		}
		return s << ")";
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const AltRE &>(other);
		auto i = 0U;
		return getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}
};

/*
 * RE_1 ; RE_2
 */
class SeqRE : public RegExp {

protected:
	template <typename... Ts> SeqRE(Ts &&...args) : RegExp() { (addKid(std::move(args)), ...); }
	SeqRE(std::vector<std::unique_ptr<RegExp>> &&kids = {}) : RegExp(std::move(kids)) {}
	SeqRE(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
	{
		addKid(std::move(r1));
		addKid(std::move(r2));
	}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<SeqRE>
	{
		return std::unique_ptr<SeqRE>(new SeqRE(std::forward<Ts>(params)...));
	}

	/* Tries to avoid creating an SeqRE if (at least) an epsilon
	 * CharRE is passed */
	template <typename... Ts> static auto createOpt(Ts &&...args) -> std::unique_ptr<RegExp>;

	auto flip() -> RegExp & override
	{
		for (auto &r : getKids()) {
			r->flip();
		}
		std::reverse(kid_begin(), kid_end());
		return *this;
	}

	[[nodiscard]] auto toNFAFast() const -> NFA override
	{
		NFA nfa;
		nfa.or_empty();
		for (const auto &k : getKids()) {
			nfa.seq(std::move(k->toNFAFast()));
		}
		return nfa;
	}

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override
	{
		NFA::State *prev = nullptr;
		for (auto i = 0U; i < getNumKids(); i++) {
			auto *s = (i == 0) ? p.first : prev;
			auto *e = (i == getNumKids() - 1) ? p.second : nfa.createState();
			getKid(i)->expandOnNFA(nfa, {s, e}, recStates);
			prev = e;
		}
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		std::vector<std::unique_ptr<RegExp>> nk;
		for (const auto &k : getKids()) {
			nk.push_back(std::move(k->clone()));
		}
		return create(std::move(nk));
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override
	{
		if (getNumKids() == 0) {
			return s << "<empty>";
		}
		s << "(";
		getKid(0)->dump(s, theory);
		for (int i = 1; i < getNumKids(); i++) {
			s << " ; ";
			getKid(i)->dump(s, theory);
		}
		return s << ")";
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const SeqRE &>(other);
		auto i = 0U;
		return getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}
};

/*******************************************************************************
 **                           Binary REs
 ******************************************************************************/

class BinaryRE : public RegExp {

protected:
	BinaryRE(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
	{
		addKid(std::move(r1));
		addKid(std::move(r2));
	}

	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const BinaryRE &>(other);
		auto i = 0U;
		return getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}
};

/*
 * RE_1 & RE_2
 */
class AndRE : public BinaryRE {

protected:
	AndRE(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
		: BinaryRE(std::move(r1), std::move(r2))
	{
	}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<AndRE>
	{
		return std::unique_ptr<AndRE>(new AndRE(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
		-> std::unique_ptr<RegExp>;

	[[nodiscard]] auto toNFAFast() const -> NFA override
	{
		std::cerr << "[Error] NFA conversion of and(&) expressions is not supported.\n";
		return {};
	}

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override
	{
		std::cerr << "[Error] NFA conversion of and(&) expressions is not supported."
			  << std::endl;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override
	{
		s << "(";
		getKid(0)->dump(s, theory);
		s << " & ";
		getKid(1)->dump(s, theory);
		return s << ")";
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const AndRE &>(other);
		auto i = 0U;
		return getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}
};

/*
 * RE_1 \ RE_2
 */
class MinusRE : public BinaryRE {

protected:
	MinusRE(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2)
		: BinaryRE(std::move(r1), std::move(r2))
	{
	}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<MinusRE>
	{
		return std::unique_ptr<MinusRE>(new MinusRE(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto toNFAFast() const -> NFA override
	{
		std::cerr << "[Error] NFA conversion of minus(\\) expressions is not supported.\n";
		return {};
	}

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override
	{
		std::cerr << "[Error] NFA conversion of minus(\\) expressions is not supported."
			  << std::endl;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override
	{
		s << "(";
		getKid(0)->dump(s, theory);
		s << " \\ ";
		getKid(1)->dump(s, theory);
		return s << ")";
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const MinusRE &>(other);
		auto i = 0U;
		return getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}
};

/*******************************************************************************
 **                         Unary operations on REs
 ******************************************************************************/

#define UNARY_RE(_class, _str)                                                                     \
	class _class##RE : public RegExp {                                                         \
                                                                                                   \
	protected:                                                                                 \
		_class##RE(std::unique_ptr<RegExp> r) : RegExp() { addKid(std::move(r)); }         \
                                                                                                   \
	public:                                                                                    \
		template <typename... Ts>                                                          \
		static std::unique_ptr<_class##RE> create(Ts &&...params)                          \
		{                                                                                  \
			return std::unique_ptr<_class##RE>(                                        \
				new _class##RE(std::forward<Ts>(params)...));                      \
		}                                                                                  \
                                                                                                   \
		static std::unique_ptr<RegExp> createOpt(std::unique_ptr<RegExp> r);               \
                                                                                                   \
		[[nodiscard]] auto toNFAFast() const -> NFA override;                              \
                                                                                                   \
		void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override;    \
                                                                                                   \
		std::unique_ptr<RegExp> clone() const override                                     \
		{                                                                                  \
			return create(getKid(0)->clone());                                         \
		}                                                                                  \
                                                                                                   \
		std::ostream &dump(std::ostream &s, const Theory *theory = nullptr) const override \
		{                                                                                  \
			getKid(0)->dump(s, theory);                                                \
			return s << _str;                                                          \
		}                                                                                  \
                                                                                                   \
	protected:                                                                                 \
		bool isEqual(const RegExp &other) const override                                   \
		{                                                                                  \
			auto &o = static_cast<const _class##RE &>(other);                          \
			auto i = 0u;                                                               \
			return getNumKids() == other.getNumKids() &&                               \
			       std::all_of(kid_begin(), kid_end(), [&](auto &k) {                  \
				       auto res = (*getKid(i) == *o.getKid(i));                    \
				       ++i;                                                        \
				       return res;                                                 \
			       });                                                                 \
		}                                                                                  \
	};

UNARY_RE(Plus, "+");
UNARY_RE(Star, "*");
UNARY_RE(QMark, "?");
UNARY_RE(Rot, "rot");

inline void PlusRE::expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const
{
	auto *s = nfa.createState();
	auto *e = nfa.createState();
	getKid(0)->expandOnNFA(nfa, {s, e}, recStates);
	nfa.addEpsilonTransition(e, s);
	nfa.addEpsilonTransition(p.first, s);
	nfa.addEpsilonTransition(e, p.second);
}

inline void StarRE::expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const
{
	auto *s = nfa.createState();
	getKid(0)->expandOnNFA(nfa, {s, s}, recStates);
	nfa.addEpsilonTransition(p.first, s);
	nfa.addEpsilonTransition(s, p.second);
}

inline void QMarkRE::expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const
{
	getKid(0)->expandOnNFA(nfa, p, recStates);
	nfa.addEpsilonTransition(p.first, p.second);
}

inline void RotRE::expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const
{
	std::cerr << "[Error] NFA conversion of rot expressions is not supported.\n";
}

inline auto PlusRE::toNFAFast() const -> NFA { return std::move(getKid(0)->toNFAFast().plus()); }
inline auto StarRE::toNFAFast() const -> NFA { return std::move(getKid(0)->toNFAFast().star()); }
inline auto QMarkRE::toNFAFast() const -> NFA
{
	return std::move(getKid(0)->toNFAFast().or_empty());
}
inline auto RotRE::toNFAFast() const -> NFA
{
	std::cerr << "[Error] NFA conversion of rot expressions is not supported.\n";
	return {};
}

/*******************************************************************************
 **                         Mutually recursive REs
 ******************************************************************************/

class MutRecRE : public RegExp {

protected:
	MutRecRE(Relation rel, std::vector<Relation> recs, std::vector<std::unique_ptr<RegExp>> res)
		: rel_(rel), recursive_(std::move(recs))
	{
		getKids() = std::move(res);
	}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<MutRecRE>
	{
		return std::unique_ptr<MutRecRE>(new MutRecRE(std::forward<Ts>(params)...));
	}

	static auto createOpt(Relation rel, std::vector<Relation> rels,
			      std::vector<std::unique_ptr<RegExp>> res) -> std::unique_ptr<RegExp>;

	[[nodiscard]] auto getRelation() const { return rel_; }

	[[nodiscard]] auto recs() const { return std::ranges::ref_view(recursive_); }

	[[nodiscard]] auto getRec(unsigned i) const { return recursive_[i]; }

	[[nodiscard]] auto toNFAFast() const -> NFA override { abort(); }

	void expandOnNFA(NFA &nfa, StatePair p, RecStatesMap recStates) const override;

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		std::vector<std::unique_ptr<RegExp>> newKids;
		for (const auto &k : getKids())
			newKids.push_back(k->clone());
		return create(rel_, recursive_, std::move(newKids));
	}

	auto dump(std::ostream &s, const Theory *theory = nullptr) const -> std::ostream & override
	{
		s << "rec@" << rel_.getID() << "[";
		for (const auto &r : recursive_)
			s << r.getID() << ",";
		s << "\b]";
		for (const auto &re : getKids()) {
			re->dump(s, theory);
			s << " & ";
		}
		s << "\b\b)";
		return s;
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const MutRecRE &>(other);
		auto i = 0U;
		return getRelation() == o.getRelation() && recursive_ == o.recursive_ &&
		       getNumKids() == other.getNumKids() &&
		       std::all_of(kid_begin(), kid_end(), [&](auto & /*k*/) {
			       auto res = (*getKid(i) == *o.getKid(i));
			       ++i;
			       return res;
		       });
	}

private:
	Relation rel_;
	std::vector<Relation> recursive_;
};

/*******************************************************************************
 **                         Helper functions
 ******************************************************************************/

template <typename OptT>
void addChildToVector(std::unique_ptr<RegExp> arg, std::vector<std::unique_ptr<RegExp>> &res)
{
	if (auto *re = dynamic_cast<const OptT *>(&*arg)) {
		for (auto i = 0U; i < arg->getNumKids(); i++) {
			res.emplace_back(arg->releaseKid(i));
		}
	} else {
		res.emplace_back(std::move(arg));
	}
}

template <typename OptT, typename... Ts>
auto createOptChildVector(Ts... args) -> std::vector<std::unique_ptr<RegExp>>
{
	std::vector<std::unique_ptr<RegExp>> res;
	(addChildToVector<OptT>(std::move(args), res), ...);
	return res;
}

template <typename... Ts> auto AltRE::createOpt(Ts &&...args) -> std::unique_ptr<RegExp>
{
	auto r = createOptChildVector<AltRE>(std::forward<Ts>(args)...);
	std::sort(r.begin(), r.end());
	r.erase(std::unique(r.begin(), r.end()), r.end());
	return r.size() == 1 ? std::move(*r.begin()) : AltRE::create(std::move(r));
}

template <typename... Ts> auto SeqRE::createOpt(Ts &&...args) -> std::unique_ptr<RegExp>
{
	auto r = createOptChildVector<SeqRE>(std::forward<Ts>(args)...);

	auto it = std::find_if(r.begin(), r.end(), [&](auto &re) { return re->isFalse(); });
	if (it != r.end()) {
		return std::move(*it);
	}

	// optimizations must be done separately; what if the user declares some combo invalid later
	// on? have a module::dump() to inspect module before and after optimizations for (auto it =
	// r.begin(); it != r.end() && it+1 != r.end(); /* */) { 	auto *p =
	// dynamic_cast<CharRE
	// *>(it->get()); 	auto *q = dynamic_cast<CharRE *>((it+1)->get()); 	if (p && q
	// && (p->getLabel().isPredicate() || q->getLabel().isPredicate())) { 		if
	// (!p->getLabel().merge(q->getLabel())) { 			return
	// RegExp::createFalse();
	// 		}
	// 		it = r.erase(it + 1);
	// 		continue;
	// 	}
	// 	++it;
	// }
	return r.size() == 1 ? std::move(*r.begin()) : SeqRE::create(std::move(r));
}

#endif /* KATER_REGEXP_HPP */
