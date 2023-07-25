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

#ifndef KATER_BASIC_PREDICATES_HPP
#define KATER_BASIC_PREDICATES_HPP

#include <algorithm>
#include <cassert>
#include <string>
#include <utility>
#include <vector>

// Stored as reversed bitmap
// FIXME: many masks do not compose; representation does not scale
enum PredicateMask : unsigned long long {
	True        = 0ULL,
	False       = ~0ULL,
	Rna         = ~(1ULL << 0),
	NRrlx       = ~(1ULL << 1),
	NRacq       = ~(1ULL << 2),
	NRsc        = ~(1ULL << 3),
	URrlx       = ~(1ULL << 4),
	URacq       = ~(1ULL << 5),
	URsc        = ~(1ULL << 6),
	DR          = ~(1ULL << 7),
	Wna         = ~(1ULL << 8),
	NWrlx       = ~(1ULL <<  9),
	NWrel       = ~(1ULL << 10),
	NWsc        = ~(1ULL << 11),
	UWrlx       = ~(1ULL << 12),
	UWrel       = ~(1ULL << 13),
	UWsc        = ~(1ULL << 14),
	DW          = ~(1ULL << 15),
	Facq        = ~(1ULL << 16),
	Frel        = ~(1ULL << 17),
	Facqrel     = ~(1ULL << 18),
	Fsc         = ~(1ULL << 19),
	Fwmb        = ~(1ULL << 20),
	Frmb        = ~(1ULL << 21),
	Fba         = ~(1ULL << 22),
	Faa         = ~(1ULL << 23),
	Fas         = ~(1ULL << 24),
	Faul        = ~(1ULL << 25),
	TC          = ~(1ULL << 26),
	TJ          = ~(1ULL << 27),
	TK          = ~(1ULL << 28),
	TB          = ~(1ULL << 29),
	TE          = ~(1ULL << 30),
	Alloc       = ~(1ULL << 31),
	Free        = ~(1ULL << 32),
	HpRetire    = ~(1ULL << 33),
	HpProtect   = ~(1ULL << 34),
	LR          = ~(1ULL << 35),
	LW          = ~(1ULL << 36),
	UL          = ~(1ULL << 37),
	RCUSync     = ~(1ULL << 38),
	RCULock     = ~(1ULL << 39),
	RCUUnlock   = ~(1ULL << 40),
	DskOpen     = ~(1ULL << 41),
	DskFsync    = ~(1ULL << 42),
	DskSync     = ~(1ULL << 43),
	DskPbarrier = ~(1ULL << 44),
	CLFlush     = ~(1ULL << 45),
	CLFlushOpt  = ~(1ULL << 46),
	IR          = ~(1ULL << 47),
	NA          = Rna   & Wna,
	NR          = Rna   & NRrlx & NRacq & NRsc,
	UR          = URrlx & URacq & URsc,
	R           = NR    & UR    & DR    & IR,
	Rrlx        = NRrlx & URrlx,
	Racq        = NRacq & URacq,
	Rsc         = NRsc  & URsc,
	Rmarked     = Rrlx  & Racq  & Rsc,
	NW          = Wna   & NWrlx & NWrel & NWsc,
	UW          = UWrlx & UWrel & UWsc,
	Wrlx        = NWrlx & UWrlx,
	Wrel        = NWrel & UWrel,
	Wsc         = NWsc  & UWsc,
	Wmarked     = Wrlx  & Wrel & Wsc,
	Marked      = Rmarked & Wmarked,
	W           = NW    & UW   & DW,
	MemAccess   = R     & W,
	F           = Frel  & Facq & Facqrel & Fsc,
	Flkmm       = Fwmb  & Frmb & Fba  & Faa & Fas & Faul,
	Acq         = Racq  & Rsc  & Facq & Facqrel & Fsc & TJ & TB,
	Rel         = Wrel  & Wsc  & Frel & Facqrel & Fsc & TC & TE,
	SC          = Rsc   & Wsc  & Fsc,
	D           = DR    & DW,
	U           = UR    & UW,
	Dep         = R     & Alloc,
	Loc         = Alloc & Free & HpRetire & MemAccess,
};

#endif /* KATER_BASIC_PREDICATES_HPP */
