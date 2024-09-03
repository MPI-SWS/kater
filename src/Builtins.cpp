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

#include "Builtins.hpp"

#include "KatModule.hpp"
#include "Predicate.hpp"
#include "Relation.hpp"
#include "TransLabel.hpp"

#include <ranges>
#include <type_traits>

using PB = Predicate::BuiltinID;
using RB = Relation::BuiltinID;

template <typename E> constexpr auto to_underlying(E e) noexcept
{
	return static_cast<std::underlying_type_t<E>>(e);
}

/************************************************************
 * Built-in relations
 ************************************************************/

static const std::unordered_map<Relation::BuiltinID, RelationInfo> builtinRelations = {
	/* po */
	{RB::po_imm,
	 {.name = "po-imm",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {},
	  .codom = {},
	  .hidden = true,
	  .genmc = {"po_imm_succ", "po_imm_pred"}}},
	{RB::po,
	 {.name = "po",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {},
	  .codom = {},
	  .genmc = {"po_succs", "po_preds"}}},
	{RB::po_loc_imm,
	 {.name = "po-loc-imm",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {},
	  .codom = {},
	  .hidden = true,
	  .genmc = {"poloc_imm_succ", "poloc_imm_pred"}}},
	{RB::po_loc,
	 {.name = "po-loc",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {},
	  .codom = {},
	  .genmc = {"poloc_succs", "poloc_preds"}}},
	{RB::rmw,
	 {.name = "rmw",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::R), Predicate::createBuiltin(PB::EXCL)},
	  .codom = {Predicate::createBuiltin(PB::W), Predicate::createBuiltin(PB::EXCL)},
	  .genmc = {"rmw_succ", "rmw_pred"}}},
	/* deps */
	{RB::ctrl,
	 {.name = "ctrl",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::DEP)},
	  .codom = {},
	  .genmc = {"?", "ctrl_preds"}}},
	{RB::addr,
	 {.name = "addr",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::DEP)},
	  .codom = {},
	  .genmc = {"?", "addr_preds"}}},
	{RB::data,
	 {.name = "data",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::DEP)},
	  .codom = {},
	  .genmc = {"?", "data_preds"}}},
	/* same thread */
	{RB::same_thread,
	 {.name = "same-thread",
	  .arity = RelArity::Conj,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {},
	  .codom = {},
	  .genmc = {"same_thread", "same_thread"}}},
	/* same location */
	{RB::alloc,
	 {.name = "alloc",
	  .arity = RelArity::OneMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::ALLOC)},
	  .codom = {},
	  .genmc = {"alloc_succs", "alloc_pred"}}},
	{RB::frees,
	 {.name = "free",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::ALLOC)},
	  .codom = {},
	  .genmc = {"free_succ", "free_pred"}}},
	// FIXME: Loc Domain and Loc codomain (Alloc,free,ret,memaccess) ++ Check basicpredicates
	// for other useful predicates?
	{RB::loc_overlap,
	 {.name = "loc-overlap",
	  .arity = RelArity::Final,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {},
	  .codom = {},
	  .genmc = {"?", "samelocs"}}},
	/* rf, co, fr, detour */
	{RB::rf,
	 {.name = "rf",
	  .arity = RelArity::OneMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::R)},
	  .genmc = {"rf_succs", "rf_pred"}}},
	{RB::rfe,
	 {.name = "rfe",
	  .arity = RelArity::OneMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::R)},
	  .genmc = {"rfe_succs", "rfe_pred"}}},
	{RB::rfi,
	 {.name = "rfi",
	  .arity = RelArity::OneMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::R)},
	  .genmc = {"rfi_succs", "rfi_pred"}}},
	{RB::tc,
	 {.name = "tc",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::TC)},
	  .codom = {Predicate::createBuiltin(PB::TB)},
	  .genmc = {"tc_succ", "tc_pred"}}},
	{RB::tj,
	 {.name = "tj",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {Predicate::createBuiltin(PB::TE)},
	  .codom = {Predicate::createBuiltin(PB::TJ)},
	  .genmc = {"tj_succ", "tj_pred"}}},
	{RB::mo_imm,
	 {.name = "mo-imm",
	  .arity = RelArity::OneOne,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .hidden = true,
	  .genmc = {"co_imm_succ", "co_imm_pred"}}},
	{RB::mo,
	 {.name = "mo",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"co_succs", "co_preds"}}},
	{RB::moe,
	 {.name = "moe",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"?", "?"}}},
	{RB::moi,
	 {.name = "moi",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"?", "?"}}},
	{RB::fr_imm,
	 {.name = "fr-imm",
	  .arity = RelArity::ManyOne,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::R)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .hidden = true,
	  .genmc = {"fr_imm_succ", "fr_imm_preds"}}},
	{RB::fr,
	 {.name = "fr",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::R)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"fr_succs", "fr_preds"}}},
	{RB::fre,
	 {.name = "fre",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::R)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"?", "?"}}},
	{RB::fri,
	 {.name = "fri",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::R)},
	  .codom = {Predicate::createBuiltin(PB::W)},
	  .genmc = {"?", "?"}}},
	{RB::detour,
	 {.name = "detour",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::KeepsLoc,
	  .dom = {Predicate::createBuiltin(PB::W)},
	  .codom = {Predicate::createBuiltin(PB::R)},
	  .genmc = {"detour_succs", "detour_preds"}}},
	/* any */
	{RB::any,
	 {.name = "any",
	  .arity = RelArity::UnsuppMany,
	  .locInfo = RelLocInfo::ChangesLoc,
	  .dom = {},
	  .codom = {},
	  .genmc = {"other_labels", "other_labels"}}},
};

static auto builtin_rel_begin() { return builtinRelations.begin(); }
static auto builtin_rel_end() { return builtinRelations.end(); }
static auto builtin_rels() { return std::ranges::ref_view(builtinRelations); }

/************************************************************
 * Built-in predicates
 ************************************************************/

static const std::unordered_map<Predicate::BuiltinID, PredicateInfo> builtinPredicates = {
	{PB::NA, {"NA", "#->isNotAtomic()"}},
	{PB::RLX, {"RLX", "#->isRelaxed()"}},
	{PB::ACQ, {"ACQ", "#->isAtLeastAcquire()"}},
	{PB::REL, {"REL", "#->isAtLeastRelease()"}},
	{PB::SC, {"SC", "#->isSC()"}},

	{PB::R, {"R", "llvm::isa<ReadLabel>(#)"}},
	{PB::W, {"W", "llvm::isa<WriteLabel>(#)"}},

	{PB::EXCL,
	 {"EXCL", "((llvm::isa<ReadLabel>(#) && g.isRMWLoad(#)) || (llvm::isa<WriteLabel>(#) && "
		  "g.isRMWStore(#)))"}},
	{PB::NEXCL,
	 {"NEXCL", "!((llvm::isa<ReadLabel>(#) && g.isRMWLoad(#)) || (llvm::isa<WriteLabel>(#) && "
		   "g.isRMWStore(#)))"}},

	{PB::F, {"F", "llvm::isa<FenceLabel>(#)"}},

	{PB::TC, {"TC", "llvm::isa<ThreadCreateLabel>(#)"}},
	{PB::TB, {"TB", "llvm::isa<ThreadStartLabel>(#)"}},
	{PB::TJ, {"TJ", "llvm::isa<ThreadJoinLabel>(#)"}},
	{PB::TK, {"TK", "llvm::isa<ThreadKillLabel>(#)"}},
	{PB::TE, {"TE", "llvm::isa<ThreadFinishLabel>(#)"}},

	{PB::ALLOC, {"ALLOC", "llvm::isa<MallocLabel>(#)"}},
	{PB::FREE, {"FREE", "llvm::isa<FreeLabel>(#) && !llvm::isa<HpRetireLabel>(#)"}},
	{PB::HPRET, {"HPRET", "llvm::isa<HpRetireLabel>(#)"}},
	{PB::HPPROT, {"HPPROT", "llvm::isa<HpProtectLabel>(#)"}},
	{PB::NOTHPPROT,
	 {"NOTHPPROT", "llvm::isa<MemAccessLabel>(#) && "
		       "llvm::dyn_cast<MemAccessLabel>(#)->getAddr().isDynamic() && "
		       "!isHazptrProtected(llvm::dyn_cast<MemAccessLabel>(#))"}},

	{PB::HEAP,
	 {"HEAP", "llvm::isa<MemAccessLabel>(#) && "
		  "llvm::dyn_cast<MemAccessLabel>(#)->getAddr().isDynamic()"}},
	{PB::REC, {"REC", "#->getThread() == g.getRecoveryRoutineId()"}},
	{PB::D,
	 {"D", "llvm::isa<MemAccessLabel>(#) && "
	       "llvm::dyn_cast<MemAccessLabel>(#)->getAddr().isDurable()"}},
	{PB::DEP, {"DEP", "#->isDependable()"}},
	{PB::LOC,
	 {"LOC", "(llvm::isa<MemAccessLabel(#) || llvm::isa<MallocLabel>(#) || "
		 "llvm::isa<FreeLabel>(#) || llvm::isa<HpProtectLabel>(#))"}},
};

static auto builtin_pred_begin() { return builtinPredicates.begin(); }
static auto builtin_pred_end() { return builtinPredicates.end(); }
static auto builtin_preds() { return std::ranges::ref_view(builtinPredicates); }

/************************************************************
 * Built-in disjoint predicate sets
 ************************************************************/

using DisjointPredSets = std::set<VSet<Predicate::ID>>;
static const DisjointPredSets disjointPredSets_ = {
	/* R, W, F, TC, TB, TE, TJ, TK, Alloc, Free */
	{to_underlying(PB::R), to_underlying(PB::W), to_underlying(PB::F), to_underlying(PB::TC),
	 to_underlying(PB::TB), to_underlying(PB::TE), to_underlying(PB::TJ), to_underlying(PB::TK),
	 to_underlying(PB::ALLOC), to_underlying(PB::FREE)},

	/* NA, RLX, {ACQ, REL} */
	{to_underlying(PB::NA), to_underlying(PB::RLX), to_underlying(PB::ACQ)},
	{to_underlying(PB::NA), to_underlying(PB::RLX), to_underlying(PB::REL)},

	/* NEXCL, EXCL */
	{to_underlying(PB::NEXCL), to_underlying(PB::EXCL)},

	/* (TB|TJ), REL */
	{to_underlying(PB::TB), to_underlying(PB::REL)},
	{to_underlying(PB::TJ), to_underlying(PB::REL)},

	/* (TC|TE), ACQ */
	{to_underlying(PB::TC), to_underlying(PB::ACQ)},
	{to_underlying(PB::TE), to_underlying(PB::ACQ)},

	/* NOTE: We don't declare e.g., W # REL because W _does_ compose with SC <= REL */

	/* DEP only composes w/ R,ALLOC */
	{to_underlying(PB::DEP), to_underlying(PB::W), to_underlying(PB::F), to_underlying(PB::TC),
	 to_underlying(PB::TB), to_underlying(PB::TE), to_underlying(PB::TJ), to_underlying(PB::TK),
	 to_underlying(PB::FREE)},

	/* LOC only composes w/ ALLOC, FREE, HPPROT, NOTHPPROT, R, W */
	{to_underlying(PB::LOC), to_underlying(PB::F), to_underlying(PB::TC), to_underlying(PB::TB),
	 to_underlying(PB::TE), to_underlying(PB::TJ), to_underlying(PB::TK)},
};

static auto disjoint_begin() { return disjointPredSets_.begin(); }
static auto disjoint_end() { return disjointPredSets_.end(); }
static auto disjoints() { return std::ranges::ref_view(disjointPredSets_); }

/************************************************************
 * Built-in predicate subsets
 ************************************************************/

using SubsetPredSets = std::unordered_map<Predicate::ID, VSet<Predicate::ID>>;
static const SubsetPredSets subsetPredSets_ = {
	{to_underlying(PB::SC), {to_underlying(PB::ACQ), to_underlying(PB::REL)}},
	{to_underlying(PB::R), {to_underlying(PB::DEP), to_underlying(PB::LOC)}},
	{to_underlying(PB::ALLOC), {to_underlying(PB::DEP), to_underlying(PB::LOC)}},
	{to_underlying(PB::FREE), {to_underlying(PB::LOC)}},
	{to_underlying(PB::HPPROT), {to_underlying(PB::LOC)}},
	{to_underlying(PB::W), {to_underlying(PB::LOC)}},
};

static auto subset_begin() { return subsetPredSets_.begin(); }
static auto subset_end() { return subsetPredSets_.end(); }
static auto subsets() { return std::ranges::ref_view(subsetPredSets_); }

/************************************************************
 * Kater theory
 ************************************************************/

/* At last, we can register all built-in knowledge (names + relevant theory) */
void registerBuiltins(KatModule &module)
{
	auto &theory = module.getTheory();

	/* Basic predicates */
	for (const auto &pi : builtin_preds()) {
		module.registerPredicate(Predicate::createBuiltin(pi.first), pi.second);
	}

	/* Sub-predicates */
	for (const auto &subset : subsets()) {
		for (const auto &p : subset.second) {
			theory.registerSubsetPair(subset.first, p);
		}
	}

	/* Invalid base compositions */
	for (const auto &disjSet : disjoints()) {
		theory.registerDisjointPreds(disjSet);
	}

	auto REFromString = [&](const std::string &s) {
		return module.getRegisteredRE(s)->clone();
	};
	auto registerBuiltinDerived = [&](const std::string &s, std::unique_ptr<RegExp> exp) {
		module.registerLet(LetStatement::create(s, std::move(exp), NoSavedExp::create(),
							std::nullopt));
	};

	/* Derived predicates */
	registerBuiltinDerived("UR", SeqRE::createOpt(REFromString("R"), REFromString("EXCL")));
	registerBuiltinDerived("UW", SeqRE::createOpt(REFromString("W"), REFromString("EXCL")));
	registerBuiltinDerived("MEM", AltRE::createOpt(REFromString("R"), REFromString("W")));

	/* Base relations */
	for (const auto &ri : builtin_rels()) {
		module.registerRelation(Relation::createBuiltin(ri.first), ri.second);
	}

	/* Per-location pairs */
	theory.registerPerLocPair(Relation::createBuiltin(Relation::BuiltinID::po_imm),
				  Relation::createBuiltin(Relation::BuiltinID::po_loc_imm));
	theory.registerPerLocPair(Relation::createBuiltin(Relation::BuiltinID::po),
				  Relation::createBuiltin(Relation::BuiltinID::po_loc));

	/* Derived relations -- currently none by default (po,etc are also built-in) */

	/* Default assumptions (ORDER MATTERS) */
	auto frmoIsFR = AssumeStatement::create(
		SubsetConstraint::createOpt(
			SeqRE::createOpt(REFromString("fr"), REFromString("mo")),
			REFromString("fr"), false, false),
		false);
	theory.registerAssume(std::move(frmoIsFR));

	auto transPO = AssumeStatement::create(
		SubsetConstraint::createOpt(
			SeqRE::createOpt(REFromString("po"), REFromString("po")),
			REFromString("po"), false, false),
		false);
	theory.registerAssume(std::move(transPO));
	auto transMO = AssumeStatement::create(
		SubsetConstraint::createOpt(
			SeqRE::createOpt(REFromString("mo"), REFromString("mo")),
			REFromString("mo"), false, false),
		false);
	theory.registerAssume(std::move(transMO));

	auto seq = SeqRE::createOpt(REFromString("rf"), REFromString("fr"));
	auto rffrIsMO = AssumeStatement::create(
		SubsetConstraint::createOpt(std::move(seq), REFromString("mo"), false, false),
		false);
	theory.registerAssume(std::move(rffrIsMO));
}
