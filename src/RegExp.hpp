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
#include "TransLabel.hpp"
#include <cassert>
#include <memory>
#include <string>
#include <typeinfo>
#include <utility>

/*******************************************************************************
 **                           RegExp Class (Abstract)
 ******************************************************************************/

class AltRE;
class Theory;

class RegExp {

protected:
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
	[[nodiscard]] virtual auto toNFA(const Theory &) const -> NFA = 0;

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
	virtual auto dump(std::ostream &s) const -> std::ostream & = 0;

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
	CharRE(const TransLabel &l) : lab(l) {}

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

	[[nodiscard]] auto toNFA(const Theory &) const -> NFA override { return NFA(lab); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getLabel());
	}

	auto dump(std::ostream &s) const -> std::ostream & override { return s << getLabel(); }

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

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		std::vector<std::unique_ptr<RegExp>> nk;
		for (const auto &k : getKids()) {
			nk.push_back(std::move(k->clone()));
		}
		return create(std::move(nk));
	}

	[[nodiscard]] auto toNFA(const Theory &theory) const -> NFA override
	{
		NFA nfa;
		for (const auto &k : getKids()) {
			nfa.alt(std::move(k->toNFA(theory)));
		}
		return nfa;
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		if (getNumKids() == 0) {
			return s << "0";
		}
		s << "(" << *getKid(0);
		for (int i = 1; i < getNumKids(); i++) {
			s << " | " << *getKid(i);
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

	[[nodiscard]] auto toNFA(const Theory &theory) const -> NFA override
	{
		NFA nfa;
		nfa.or_empty();
		for (const auto &k : getKids()) {
			nfa.seq(std::move(k->toNFA(theory)));
		}
		return nfa;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		std::vector<std::unique_ptr<RegExp>> nk;
		for (const auto &k : getKids()) {
			nk.push_back(std::move(k->clone()));
		}
		return create(std::move(nk));
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		if (getNumKids() == 0) {
			return s << "<empty>";
		}
		s << "(" << *getKid(0);
		for (int i = 1; i < getNumKids(); i++) {
			s << " ; " << *getKid(i);
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

	[[nodiscard]] auto toNFA(const Theory &theory) const -> NFA override
	{
		std::cerr << "[Error] NFA conversion of and(&) expressions is not supported."
			  << std::endl;
		NFA nfa1 = getKid(0)->toNFA(theory);
		return nfa1;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "(" << *getKid(0) << " & " << *getKid(1) << ")";
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

	[[nodiscard]] auto toNFA(const Theory &theory) const -> NFA override
	{
		std::cerr << "[Error] NFA conversion of minus(\\) expressions is not supported."
			  << std::endl;
		NFA nfa1 = getKid(0)->toNFA(theory);
		return nfa1;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "(" << *getKid(0) << " \\ " << *getKid(1) << ")";
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

#define UNARY_RE(_class, _op, _str)                                                                \
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
		NFA toNFA(const Theory &theory) const override                                     \
		{                                                                                  \
			NFA nfa = getKid(0)->toNFA(theory);                                        \
			nfa._op();                                                                 \
			return nfa;                                                                \
		}                                                                                  \
                                                                                                   \
		std::unique_ptr<RegExp> clone() const override                                     \
		{                                                                                  \
			return create(getKid(0)->clone());                                         \
		}                                                                                  \
                                                                                                   \
		std::ostream &dump(std::ostream &s) const override                                 \
		{                                                                                  \
			return s << *getKid(0) << _str;                                            \
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

UNARY_RE(Plus, plus, "+");
UNARY_RE(Star, star, "*");
UNARY_RE(QMark, or_empty, "?");

class RotRE : public RegExp {

protected:
	RotRE(std::unique_ptr<RegExp> r) { addKid(std::move(r)); }

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<RotRE>
	{
		return std::unique_ptr<RotRE>(new RotRE(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> r) -> std::unique_ptr<RegExp>;

	[[nodiscard]] auto toNFA(const Theory &theory) const -> NFA override;

	[[nodiscard]] auto clone() const -> std::unique_ptr<RegExp> override
	{
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "rot(" << *getKid(0) << ")";
	}

protected:
	[[nodiscard]] auto isEqual(const RegExp &other) const -> bool override
	{
		const auto &o = dynamic_cast<const RotRE &>(other);
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
	// r.begin(); it != r.end() && it+1 != r.end(); /* */) { 	auto *p = dynamic_cast<CharRE
	// *>(it->get()); 	auto *q = dynamic_cast<CharRE *>((it+1)->get()); 	if (p && q &&
	// (p->getLabel().isPredicate() || q->getLabel().isPredicate())) { 		if
	// (!p->getLabel().merge(q->getLabel())) { 			return RegExp::createFalse();
	// 		}
	// 		it = r.erase(it + 1);
	// 		continue;
	// 	}
	// 	++it;
	// }
	return r.size() == 1 ? std::move(*r.begin()) : SeqRE::create(std::move(r));
}

#endif /* KATER_REGEXP_HPP */
