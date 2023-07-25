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

#include "Predicate.hpp"
#include "BasicPredicates.hpp"
#include <cassert>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

//	{Predicate::IsDynamicLoc, "g.is_dynamic_loc(#)"},
	// { PredicateMask::NotHpProtected, {"NotHpProtected", "llvm::isa<MemAccessLabel>(#) && !g.isHazptrProtected(#)"}},
//	{Predicate::RfInit,	 "llvm::isa<ReadLabel>(#) && llvm::dyn_cast<ReadLabel>(#)->getRf().isInitializer()"},
//	{Predicate::REC,	 "#->getThread() == g.getRecoveryRoutineId()"},

//
// FIXME: Name printing need fixing (+ remove redundancy)
//
const std::vector<std::pair<PredicateMask, PredicateInfo>> PredicateSet::builtins = {
	{ PredicateMask::MemAccess,  {"MEM"      , "llvm::isa<MemAccessLabel>(#)"}},
	{ PredicateMask::R,          {"R"        , "llvm::isa<ReadLabel>(#)"}},
	{ PredicateMask::W,          {"W"        , "llvm::isa<WriteLabel>(#)"}},
	{ PredicateMask::F,          {"F"        , "llvm::isa<FenceLabel>(#)"}},
	{ PredicateMask::NA,         {"NA"       , "#->isNotAtomic()"}},
	{ PredicateMask::Marked,     {"Marked"   , "!#->isNotAtomic()"}},
	{ PredicateMask::Acq,        {"ACQ"      , "#->isAtLeastAcquire()"}},
	{ PredicateMask::Rel,        {"REL"      , "#->isAtLeastRelease()"}},
	{ PredicateMask::SC,         {"SC"       , "#->isSC()"}},
	{ PredicateMask::D,          {"D"        , "llvm::isa<MemAccessLabel>(#) && llvm::dyn_cast<MemAccessLabel>(#)->getAddr().isDurable()"}},
	{ PredicateMask::Dep,        {"Dep"      , "#->isDependable()"}},
	{ PredicateMask::UR,         {"UR"       , "g.isRMWLoad(#)"}},
	{ PredicateMask::UW,         {"UW"       , "g.isRMWStore(#)"}},
	{ PredicateMask::NR,         {"R\\U"     , "llvm::isa<ReadLabel>(#) && !g.isRMWLoad(#)"}},
	{ PredicateMask::NW,         {"W\\U"     , "llvm::isa<WriteLabel>(#) && !g.isRMWStore(#)"}},
	{ PredicateMask::Rmarked,    {"R&Marked" , "llvm::isa<ReadLabel>(#) && !#->isNotAtomic()"}},
	{ PredicateMask::Rna,        {"Rna"      , "llvm::isa<ReadLabel>(#) && #->isNotAtomic()"}},
	{ PredicateMask::Rrlx,       {"Rrlx"     , "llvm::isa<ReadLabel>(#) && !#->isNotAtomic() && !#->isAtLeastAcquire()"}},
	{ PredicateMask::Racq,       {"Racq"     , "llvm::isa<ReadLabel>(#) && #->isAtLeastAcquire() && !#->isSC()"}},
	{ PredicateMask::Rsc,        {"Rsc"      , "llvm::isa<ReadLabel>(#) && #->isSC()"}},
	{ PredicateMask::Wmarked,    {"W&Marked" , "llvm::isa<WriteLabel>(#) && !#->isNotAtomic()"}},
	{ PredicateMask::Wna,        {"Wna"      , "llvm::isa<WriteLabel>(#) && #->isNotAtomic()"}},
	{ PredicateMask::Wrlx,       {"Wrlx"     , "llvm::isa<WriteLabel>(#) && !#->isNotAtomic() && !#->isAtLeastRelease()"}},
	{ PredicateMask::Wrel,       {"Wrel"     , "llvm::isa<WriteLabel>(#) && #->isAtLeastRelease() && !#->isSC()"}},
	{ PredicateMask::Wsc,        {"Wsc"      , "llvm::isa<WriteLabel>(#) && #->isSC()"}},
	{ PredicateMask::NRrlx,      {"NRrlx"    , "llvm::isa<ReadLabel>(#) && !g.isRMWLoad(#) && ???"}},
	{ PredicateMask::NRacq,      {"NRacq"    , "llvm::isa<ReadLabel>(#) && !g.isRMWLoad(#) && ???"}},
	{ PredicateMask::NRsc,       {"NRsc"     , "llvm::isa<ReadLabel>(#) && !g.isRMWLoad(#) && #->isSC()"}},
	{ PredicateMask::URrlx,      {"URrlx"    , "llvm::isa<ReadLabel>(#) && g.isRMWLoad(#) && ???"}},
	{ PredicateMask::URacq,      {"URacq"    , "llvm::isa<ReadLabel>(#) && g.isRMWLoad(#) && ???"}},
	{ PredicateMask::URsc,       {"URsc"     , "llvm::isa<ReadLabel>(#) && g.isRMWLoad(#) && #->isSC()"}},
	{ PredicateMask::DR,         {"DR"       , "??"}},
	{ PredicateMask::NWrlx,      {"NWrlx"    , "llvm::isa<WriteLabel>(#) && !g.isRMWStore(#) && #->isRelaxed()"}},
	{ PredicateMask::NWrel,      {"NWrel"    , "llvm::isa<WriteLabel>(#) && !g.isRMWStore(#) && #->isAcquire()"}},
	{ PredicateMask::NWsc,       {"NWsc"     , "llvm::isa<WriteLabel>(#) && !g.isRMWStore(#) && #->isSC()"}},
	{ PredicateMask::UWrlx,      {"UWrlx"    , "llvm::isa<WriteLabel>(#) && g.isRMWStore(#) && #->isRelaxed()"}},
	{ PredicateMask::UWrel,      {"UWrel"    , "llvm::isa<WriteLabel>(#) && g.isRMWStore(#) && #->isRelease()"}},
	{ PredicateMask::UWsc,       {"UWsc"     , "llvm::isa<WriteLabel>(#) && g.isRMWStore(#) && #->isSC()"}},
	{ PredicateMask::DW,         {"DW"       , "??"}},
	{ PredicateMask::Facq,       {"Facq"     , "llvm::isa<FenceLabel>(#) && #->isAtLeastAcquire() && !#->isAtLeastRelease()"}},
	{ PredicateMask::Frel,       {"Frel"     , "llvm::isa<FenceLabel>(#) && #->isAtLeastRelease() && !#->isAtLeastAcquire()" }},
	{ PredicateMask::Facqrel,    {"Facqrel"  , "llvm::isa<FenceLabel>(#) && #->isAtLeastAcquire() && #->isAtLeastRelease()"}},
	{ PredicateMask::Fsc,        {"Fsc"      , "llvm::isa<FenceLabel>(#) && #->isSC()"}},
	{ PredicateMask::Fwmb,       {"Fwmb"     , "SmpFenceLabelLKMM::isType(#, SmpFenceType::WMB)"}},
	{ PredicateMask::Frmb,       {"Frmb"     , "SmpFenceLabelLKMM::isType(#, SmpFenceType::RMB)"}},
	{ PredicateMask::Fba,        {"Fba"      , "SmpFenceLabelLKMM::isType(#, SmpFenceType::MBBA)"}},
	{ PredicateMask::Faa,        {"Faa"      , "SmpFenceLabelLKMM::isType(#, SmpFenceType::MBAA)"}},
	{ PredicateMask::Fas,        {"Fas"      , "SmpFenceLabelLKMM::isType(#, SmpFenceType::MBAS)"}},
	{ PredicateMask::Faul,       {"Faul"     , "SmpFenceLabelLKMM::isType(#, SmpFenceType::MBAUL)"}},
	{ PredicateMask::TC,         {"TC"       , "llvm::isa<ThreadCreateLabel>(#)"}},
	{ PredicateMask::TB,         {"TB"       , "llvm::isa<ThreadStartLabel>(#)"}},
	{ PredicateMask::TJ,         {"TJ"       , "llvm::isa<ThreadJoinLabel>(#)"}},
	{ PredicateMask::TK,         {"TK"       , "llvm::isa<ThreadKillLabel>(#)"}},
	{ PredicateMask::TE,         {"TE"       , "llvm::isa<ThreadFinishLabel>(#)"}},
	{ PredicateMask::Alloc,      {"Alloc"    , "llvm::isa<MallocLabel>(#)"}},
	{ PredicateMask::Free,       {"Free"     , "llvm::isa<FreeLabel>(#) && !llvm::isa<HpRetireLabel>(#)"}},
	{ PredicateMask::HpRetire,   {"HpRetire" , "llvm::isa<HpRetireLabel>(#)"}},
	{ PredicateMask::HpProtect,  {"HpProtect", "llvm::isa<HpProtectLabel>(#)"}},
	{ PredicateMask::LR,         {"LR"       , "llvm::isa<LockCasReadLabel>(#)"}},
	{ PredicateMask::LW,         {"LW"       , "llvm::isa<LockCasWriteLabel>(#)"}},
	{ PredicateMask::UL,         {"UL"       , "llvm::isa<UnlockWriteLabel>(#)"}},
	{ PredicateMask::RCUSync,    {"RCUSync"  , "llvm::isa<RCUSyncLabel>(#)"}},
	{ PredicateMask::RCULock,    {"RCULock"  , "llvm::isa<RCULockLabel>(#)"}},
	{ PredicateMask::RCUUnlock,  {"RCUUnlock", "llvm::isa<RCUUnlockLabel>(#)"}},
	{ PredicateMask::DskOpen,    {"DskOpen"  , "llvm::isa<DskOpenLabel>(#)"}},
	{ PredicateMask::DskFsync,   {"DskFsync" , "llvm::isa<DskFsyncLabel>(#)"}},
	{ PredicateMask::DskSync,    {"DskSync"  , "llvm::isa<DskSyncLabel>(#)"}},
	{ PredicateMask::DskPbarrier,{"DskPbarrier", "llvm::isa<DskPbarrierLabel>(#)"}},
	{ PredicateMask::CLFlush,    {"CLFlush"    , "llvm::isa<CLFlushLabel>(#)"}},
	{ PredicateMask::CLFlushOpt, {"CLFlushOpt" , "llvm::isa<CLFlushOptLabel>(#)"}},
	{ PredicateMask::IR,         {"IR"       , "llvm::isa<ReadLabel>(#) && llvm::dyn_cast<ReadLabel>(#)->getRf() && llvm::dyn_cast<ReadLabel>(#)->getRf()->getPos().isInitializer() && llvm::dyn_cast<ReadLabel>(#)->getAddr().isDynamic()"}}};

static inline auto operator|(PredicateMask m1, PredicateMask m2) -> PredicateMask
{
	return static_cast<PredicateMask>(static_cast<unsigned long long>(m1)
                                        | static_cast<unsigned long long>(m2));
}

static inline auto operator&(PredicateMask m1, PredicateMask m2) -> PredicateMask
{
	return static_cast<PredicateMask>(static_cast<unsigned long long>(m1)
                                        & static_cast<unsigned long long>(m2));
}

static inline auto operator~(PredicateMask m) -> PredicateMask
{
	return static_cast<PredicateMask>(~static_cast<unsigned long long>(m));
}

auto operator<<(std::ostream& ostr, const PredicateSet &s) -> std::ostream &
{
	if (s.mask == PredicateMask::True) { ostr << "[]"; return ostr; }
	ostr << "[";
	auto m = s.mask;
	auto first = true;
	for (const auto &p : PredicateSet::builtins) {
		auto mm = static_cast<PredicateMask>(p.first);
		if ((mm | s.mask) != mm) {
			continue;
		}
		if ((mm | m) == PredicateMask::False) {
			continue;
		}
		m = m | ~mm;
		ostr << (!first ? "|" : "") << p.second.name;
		first = false;
	}
	ostr << "]";
	return ostr;
}

auto PredicateSet::toGenmcString() const -> std::string
{
	auto m = mask;
	if (m == PredicateMask::True) {
		return "";
	}
	auto first = true;
	std::string ostr;
	for (const auto &p : builtins) {
		auto mm = static_cast<PredicateMask>(p.first);
		if ((mm | mask) != mm) {
			continue;
		}
		if ((mm | m) == PredicateMask::False) {
			continue;
		}
		m = m | ~mm;
		if (!first) {
			ostr += " || ";
		}
		ostr += p.second.genmcString;
		first = false;
	}
	return ostr;
}

auto PredicateSet::empty() const -> bool
{
	return mask == PredicateMask::True;
}
auto PredicateSet::includes(const PredicateSet &other) const -> bool
{
	return (mask | other.mask) == other.mask;
}

auto PredicateSet::composes(const PredicateSet &other) const -> bool
{
	return (mask | other.mask) != PredicateMask::False;
}

auto PredicateSet::merge(const PredicateSet &other) -> bool
{
	if (!composes(other)) { return false;
}
	mask = mask | other.mask;
	return true;
}

void PredicateSet::minus(const PredicateSet &other)
{
	mask = mask | ~other.mask;
}
