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

#ifndef KATER_REGEXP_UTILS_HPP
#define KATER_REGEXP_UTILS_HPP

#include "RegExp.hpp"

#include <memory>

/* Does a post-order visitation of R, applying FUN */
template <typename F> static inline void visitRE(std::unique_ptr<RegExp> &r, F &&fun)
{
	for (auto i = 0U; i < r->getNumKids(); i++) {
		visitRE(r->getKid(i), fun);
	}
	fun(r);
}

static inline void replaceREWith(std::unique_ptr<RegExp> &r, const RegExp *from, const RegExp *to)
{
	visitRE(r, [&](auto &r) {
		if (*r == *from)
			r = to->clone();
	});
}

/* Returns a copy of RE with every `any` (Univ) sub-expression replaced by `any*`. */
static inline auto starifyAny(std::unique_ptr<RegExp> re) -> std::unique_ptr<RegExp>
{
	if (re->isAnyRelation())
		return StarRE::createOpt(std::move(re));
	for (auto i = 0U; i < re->getNumKids(); ++i)
		re->setKid(i, starifyAny(re->releaseKid(i)));
	return re;
}

#endif /* KATER_REGEXP_UTILS_HPP */
