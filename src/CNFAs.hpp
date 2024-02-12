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

#include <optional>
#include <vector>

/*
 * Constrained NFAs class
 * A collection of NFAs representing different constraint types
 */
// TODO: Delete class altogether
class CNFAs {

public:
	CNFAs() = default;

	[[nodiscard]] auto getRecovery() const -> const NFA & { return rec; }

	[[nodiscard]] auto getPPoRf() const -> const std::pair<NFA, bool> & { return pporf; }

	auto save_begin() { return nsave.begin(); }
	auto save_end() { return nsave.end(); }
	[[nodiscard]] auto save_begin() const { return nsave.begin(); }
	[[nodiscard]] auto save_end() const { return nsave.end(); }

	auto incl_begin() { return nincl.begin(); }
	auto incl_end() { return nincl.end(); }
	[[nodiscard]] auto incl_begin() const { return nincl.begin(); }
	[[nodiscard]] auto incl_end() const { return nincl.end(); }

	auto acyc_begin() { return acyc.begin(); }
	auto acyc_end() { return acyc.end(); }
	auto acycs() { return std::ranges::ref_view(acyc); }
	[[nodiscard]] auto acyc_begin() const { return acyc.begin(); }
	[[nodiscard]] auto acyc_end() const { return acyc.end(); }
	[[nodiscard]] auto acycs() const { return std::ranges::ref_view(acyc); }

	void addAcyclic(NFA &&a, std::string genmc = {}, std::optional<NFA> unless = std::nullopt)
	{
		acyc.emplace_back(std::move(a), std::move(genmc), std::move(unless));
	}

	void addRecovery(NFA &&a) { rec.alt(std::move(a)); }

	void addPPoRf(NFA &&ppo, bool deps = false)
	{
		pporf = std::make_pair(std::move(ppo), deps);
	}

	void addSaved(NFA &&save) { nsave.emplace_back(std::move(save), VarStatus::Normal); }

	void addReduced(NFA &&redc) { nsave.emplace_back(std::move(redc), VarStatus::Reduce); }

	void addView(NFA &&view) { nsave.emplace_back(std::move(view), VarStatus::View); }

	void addInclusion(std::pair<Inclusion<NFA>, int> &&incl)
	{
		nincl.push_back(std::move(incl));
	}

	void setCohIndex(int idx) { cohIndex = idx; }
	[[nodiscard]] auto getCohIndex() const -> int { return cohIndex; }

	void setHbIndex(int idx) { hbIndex = idx; }
	[[nodiscard]] auto getHbIndex() const -> int { return hbIndex; }

	[[nodiscard]] auto isDepTracking() const -> bool { return depTracking; }
	void setDepTracking(bool dt) { depTracking = dt; }

private:
	std::vector<std::tuple<NFA, std::string, std::optional<NFA>>>
		acyc; // opt: possible unless clause
	NFA rec;
	std::pair<NFA, bool> pporf; // 1 -> deps
	std::vector<std::pair<NFA, VarStatus>> nsave;
	// std::vector<NFA> nredc;
	std::vector<std::pair<Inclusion<NFA>, int>>
		nincl; // index of rhs view; -1 if none ** FIXME **

	int cohIndex = -1;
	int hbIndex = -1;
	bool depTracking = false;
};

#endif /* KATER_CNFAS_HPP */
