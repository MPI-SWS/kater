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

#include "Relation.hpp"
#include "BasicPredicates.hpp"

using PB = PredicateMask;
const std::unordered_map<Relation::Builtin, RelationInfo> Relation::builtins = {
        /* po */
        {po_imm,	{"po-imm",      RelType::OneOne,     {},   {},   true}},
        {po_loc_imm,	{"po-loc-imm",  RelType::OneOne,     {},   {},   true}},
	/* deps */
        {ctrl_imm,	{"ctrl-imm",    RelType::UnsuppMany, {(PB::Dep)}, {},   true}},
        {addr_imm,	{"addr-imm",    RelType::UnsuppMany, {(PB::Dep)}, {},   true}},
        {data_imm,	{"data-imm",    RelType::UnsuppMany, {(PB::Dep)}, {},   true}},
	/* same thread */
	{same_thread,	{"same-thread", RelType::Conj,	     {},   {},   false}},
	/* same location */
        {alloc,		{"alloc",       RelType::ManyOne,    {},   {},   false}},
        {frees,		{"frees",       RelType::OneOne,     {},   {},   false}},
        {loc_overlap,	{"loc-overlap", RelType::Final,      {(PB::Loc)},   {(PB::Loc)},   false}},
	/* rf, co, fr, detour */
        {rf,		{"rf",          RelType::ManyOne,    {(PB::W)},  {(PB::R)}, false}},
        {rfe,		{"rfe",         RelType::ManyOne,    {(PB::W)},  {(PB::R)}, false}},
        {rfi,		{"rfi",         RelType::ManyOne,    {(PB::W)},  {(PB::R)}, false}},
        {tc,		{"tc",          RelType::OneOne,     {(PB::TC)}, {(PB::TB)}, false}},
        {tj,		{"tj",          RelType::OneOne,     {(PB::TE)}, {(PB::TJ)}, false}},
        {mo_imm,	{"mo-imm",      RelType::OneOne,     {(PB::W)},  {(PB::W)}, false}},
        {moe,		{"moe",         RelType::UnsuppMany, {(PB::W)},  {(PB::W)}, false}},
        {moi,		{"moi",         RelType::UnsuppMany, {(PB::W)},  {(PB::W)}, false}},
        {fr_imm,	{"fr-imm",      RelType::ManyOne,    {(PB::R)},  {(PB::W)}, false}},
        {fre,		{"fre",         RelType::ManyOne,    {(PB::R)},  {(PB::W)}, false}},
        {fri,		{"fri",         RelType::ManyOne,    {(PB::R)},  {(PB::W)}, false}},
        {detour,	{"detour",      RelType::OneOne,     {(PB::W)},  {(PB::R)}, false}},
	/* any */
        {any,		{"any",         RelType::OneOne,     {},  {}, false}},
};

std::unordered_map<Relation::ID, Relation> Relation::perlocs = {
	{po_imm, po_loc_imm},
};

auto Relation::getDomain() const -> const PredicateSet &
{
	assert(isBuiltin());
	return isInverse() ? builtins.find(toBuiltin())->second.codom :
		builtins.find(toBuiltin())->second.dom;
}

auto Relation::getCodomain() const -> const PredicateSet &
{
	assert(isBuiltin());
	return isInverse() ? builtins.find(toBuiltin())->second.dom :
		builtins.find(toBuiltin())->second.codom;
}

auto Relation::includes(const Relation &other) const -> bool
{
	if (*this == other) {
		return true;
	}

	/* r <= any */
	if (isBuiltin() && toBuiltin() == Builtin::any) {
		return true;
	}
	if (other.isBuiltin() && other.toBuiltin() == Builtin::any) {
		return false;
	}

	/* flipped? */
	if (isInverse() != other.isInverse()) {
		return false;
	}

	/* r&loc <= r */
	if (hasPerLoc() && getPerLoc() == other) {
		return true;
	}

	/* special cases for builtins */
	if (!isBuiltin() || !other.isBuiltin()) {
		return false;
	}

	auto lb = toBuiltin();
	auto rb = other.toBuiltin();

	/* rfe, rfi <= rf */
	if (lb == Builtin::rf && (rb == Builtin::rfi || rb == Builtin::rfe)) {
		return true;
	}

	/* moi, rfi, fri <= po,po-loc */
	if ((lb == Builtin::po_imm || lb == Builtin::po_loc_imm) &&
	    Builtin::WithinPoBegin <= rb && rb <= Builtin::WithinPoLast) {
		return true;
	}

	return false;
}

auto Relation::getName() const -> std::string
{
	return (isBuiltin() ? Relation::builtins.find(toBuiltin())->second.name :
		("$" + std::to_string(getID()))) +
		(isInverse() ? "-1" : "");
}
