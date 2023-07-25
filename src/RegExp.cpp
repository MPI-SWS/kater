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

#include "RegExp.hpp"
#include "Saturation.hpp"

auto RegExp::createFalse() -> std::unique_ptr<RegExp>
{
	return AltRE::create();
}

auto RegExp::createId() -> std::unique_ptr<RegExp>
{
	TransLabel t (std::nullopt);
	return CharRE::create(t);
}

auto RegExp::createSym(std::unique_ptr<RegExp> re) -> std::unique_ptr<RegExp>
{
	auto re1 = re->clone();
	re1->flip();
	return AltRE::createOpt(std::move(re), std::move(re1));
}

auto RegExp::isFalse() const -> bool
{
	return getNumKids() == 0 && (dynamic_cast<const AltRE *>(this) != nullptr);
}

auto RegExp::getDomain() const -> std::unique_ptr<RegExp>
{
	if (const auto *charRE = dynamic_cast<const CharRE *>(&*this)) {
		TransLabel p (std::nullopt, charRE->getLabel().getPreChecks(), {});
		return CharRE::create(p);
	}
	return createId();
}

auto RegExp::getCodomain() const -> std::unique_ptr<RegExp>
{
	if (const auto *charRE = dynamic_cast<const CharRE *>(&*this)) {
		TransLabel p (std::nullopt, charRE->getLabel().getPostChecks(), {});
		return CharRE::create(p);
	}
	return createId();
}

auto
PlusRE::createOpt(std::unique_ptr<RegExp> r) -> std::unique_ptr<RegExp>
{
	if ((dynamic_cast<PlusRE *>(&*r) != nullptr) || (dynamic_cast<StarRE *>(&*r) != nullptr)) {
		return std::move(r);
	}
	if (dynamic_cast<QMarkRE *>(&*r) != nullptr) {
		return StarRE::createOpt(r->releaseKid(0));
	}
	return create(std::move(r));
}

auto
StarRE::createOpt(std::unique_ptr<RegExp> r) -> std::unique_ptr<RegExp>
{
	if (dynamic_cast<StarRE *>(&*r) != nullptr) {
		return std::move(r);
	}
	if ((dynamic_cast<PlusRE *>(&*r) != nullptr) || (dynamic_cast<QMarkRE *>(&*r) != nullptr)) {
		return create(r->releaseKid(0));
	}
	return create(std::move(r));
}

auto
QMarkRE::createOpt(std::unique_ptr<RegExp> r) -> std::unique_ptr<RegExp>
{
	if ((dynamic_cast<QMarkRE *>(&*r) != nullptr) || (dynamic_cast<StarRE *>(&*r) != nullptr)) {
		return std::move(r);
	}
	if (dynamic_cast<PlusRE *>(&*r) != nullptr) {
		return StarRE::createOpt(r->releaseKid(0));
	}
	return create(std::move(r));
}

static auto isPredicate (const RegExp &r) -> bool
{
	if (const auto *charRE = dynamic_cast<const CharRE *>(&r)) {
		return charRE->getLabel().isPredicate();
	}
	return false;
}

static auto isAnyRel (const RegExp &r) -> bool
{
	if (const auto *charRE = dynamic_cast<const CharRE *>(&r)) {
		if (auto rel = charRE->getLabel().getRelation()) {
			return (rel->isBuiltin() && rel->toBuiltin() == Relation::any);
		}
	}
	return false;
}

auto
AndRE::createOpt(std::unique_ptr<RegExp> r1, std::unique_ptr<RegExp> r2) -> std::unique_ptr<RegExp>
{
	// `r & r = r`
	if (*r1 == *r2) {
		return std::move(r1);
	}
	// `pred & pred2 =  pred1 ; pred2`
	if (isPredicate(*r1) && isPredicate(*r2)) {
		return SeqRE::createOpt(std::move(r1), std::move(r2));
	}
	// `([pred] ; any ; [pred2]) & r = [pred1] ; r ; [pred2]`
	if (isAnyRel(*r1)) {
		return SeqRE::createOpt(r1->getDomain(), std::move(r2), r1->getCodomain());
	}
	// `r & ([pred] ; any ; [pred2]) = [pred1] ; r ; [pred2]`
	if (isAnyRel(*r2)) {
		return SeqRE::createOpt(r2->getDomain(), std::move(r1), r2->getCodomain());
	}
	return create(std::move(r1), std::move(r2));
}

auto
RotRE::createOpt(std::unique_ptr<RegExp> r) -> std::unique_ptr<RegExp>
{
	if (dynamic_cast<RotRE *>(&*r) != nullptr) {
		return std::move(r);
	}
	return create(std::move(r));
}

auto RotRE::toNFA() const -> NFA
{
	auto nfa = getKid(0)->toNFA();
	saturateRotate(nfa);
	return nfa;
}
