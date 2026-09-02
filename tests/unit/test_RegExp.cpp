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
 *
 */

#include "RegExp.hpp"
#include "Relation.hpp"
#include "TransLabel.hpp"

#include <gtest/gtest.h>

#include <memory>

/* Returns a CharRE for the builtin relation REL */
static auto builtin(Relation::BuiltinID rel) -> std::unique_ptr<RegExp>
{
	return CharRE::create(TransLabel(Relation::createBuiltin(rel)));
}

using BID = Relation::BuiltinID;

TEST(AltRECreateOpt, DropsDuplicateAlternatives)
{
	auto re = AltRE::createOpt(builtin(BID::po), builtin(BID::rf), builtin(BID::po));

	/* `po | rf | po` is `po | rf` */
	ASSERT_EQ(re->getNumKids(), 2U);
	EXPECT_EQ(*re->getKid(0), *builtin(BID::po));
	EXPECT_EQ(*re->getKid(1), *builtin(BID::rf));
}

TEST(AltRECreateOpt, CollapsesToASingleAlternative)
{
	auto re = AltRE::createOpt(builtin(BID::po), builtin(BID::po));

	EXPECT_EQ(*re, *builtin(BID::po));
}

TEST(AltRECreateOpt, KeepsTheGivenOrder)
{
	auto re = AltRE::createOpt(builtin(BID::rf), builtin(BID::po), builtin(BID::mo));

	/* The order must not depend on where the kids happen to be allocated */
	ASSERT_EQ(re->getNumKids(), 3U);
	EXPECT_EQ(*re->getKid(0), *builtin(BID::rf));
	EXPECT_EQ(*re->getKid(1), *builtin(BID::po));
	EXPECT_EQ(*re->getKid(2), *builtin(BID::mo));
}
