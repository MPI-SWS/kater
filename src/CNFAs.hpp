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

#ifndef KATER_CNFAS_HPP
#define KATER_CNFAS_HPP

#include "Inclusion.hpp"
#include "KatModuleAPI.hpp"
#include "NFA.hpp"

/*
 * Constrained NFAs class
 * A collection of NFAs representing different constraint types
 */
// TODO: Delete class altogether
class CNFAs {

public:
	CNFAs() = default;

	using save_iter = std::vector<std::pair<NFA, VarStatus>>::iterator;
	using save_const_iter = std::vector<std::pair<NFA, VarStatus>>::const_iterator;
	using redc_iter = save_iter;
	using redc_const_iter = save_const_iter;
	using incl_iter = std::vector<std::pair<Inclusion<NFA>,int>>::iterator;
	using incl_const_iter = std::vector<std::pair<Inclusion<NFA>,int>>::const_iterator;

	[[nodiscard]] auto getAcyclic() const -> const NFA & { return acyc; }

	[[nodiscard]] auto getRecovery() const -> const NFA & { return rec; }

	[[nodiscard]] auto getPPoRf() const -> const std::pair<NFA, bool> & { return pporf; }

	auto save_begin() -> save_iter { return nsave.begin(); }
	auto save_end() -> save_iter { return nsave.end(); }
	[[nodiscard]] auto save_begin() const -> save_const_iter { return nsave.begin(); }
	[[nodiscard]] auto save_end() const -> save_const_iter { return nsave.end(); }

	// redc_iter redc_begin() { return nredc.begin(); }
	// redc_iter redc_end() { return nredc.end(); }
	// redc_const_iter redc_begin() const { return nredc.begin(); }
	// redc_const_iter redc_end() const { return nredc.end(); }

	auto incl_begin() -> incl_iter { return nincl.begin(); }
	auto incl_end() -> incl_iter { return nincl.end(); }
	[[nodiscard]] auto incl_begin() const -> incl_const_iter { return nincl.begin(); }
	[[nodiscard]] auto incl_end() const -> incl_const_iter { return nincl.end(); }

	void addAcyclic(NFA &&a) { acyc.alt(std::move(a)); }

	void addRecovery(NFA &&a) { rec.alt(std::move(a)); }

	void addPPoRf(NFA &&ppo, bool deps = false) { pporf = std::make_pair(std::move(ppo), deps); }

	void addSaved(NFA &&save) { nsave.emplace_back(std::move(save), VarStatus::Normal); }

	void addReduced(NFA &&redc) { nsave.emplace_back(std::move(redc), VarStatus::Reduce); }

	void addView(NFA &&view) { nsave.emplace_back(std::move(view), VarStatus::View); }

	void addInclusion(std::pair<Inclusion<NFA>, int> &&incl) { nincl.push_back(std::move(incl)); }

	void setCohIndex(int idx) { cohIndex = idx; }
	[[nodiscard]] auto getCohIndex() const -> int { return cohIndex; }

	void setHbIndex(int idx) { hbIndex = idx; }
	[[nodiscard]] auto getHbIndex() const -> int { return hbIndex; }

	[[nodiscard]] auto isDepTracking() const -> bool { return depTracking; }
	void setDepTracking(bool dt) { depTracking = dt; }

private:
	NFA acyc;
	NFA rec;
	std::pair<NFA, bool> pporf; // 1 -> deps
	std::vector<std::pair<NFA, VarStatus>> nsave;
	// std::vector<NFA> nredc;
	std::vector<std::pair<Inclusion<NFA>, int>> nincl; // index of rhs view; -1 if none ** FIXME **

	int cohIndex = -1;
	int hbIndex = -1;
	bool depTracking = false;
};

#endif /* KATER_CNFAS_HPP */
