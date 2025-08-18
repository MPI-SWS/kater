/*
 * GenMC -- Generic Model Checking.
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
 * Author: Michalis Kokologiannakis <michalis@mpi-sws.org>
 */

/*******************************************************************************
 * CAUTION: This file is generated automatically by Kater -- DO NOT EDIT.
 *******************************************************************************/

#include "RISCVDriver.hpp"
#include "Static/ModuleInfo.hpp"

RISCVDriver::RISCVDriver(std::shared_ptr<const Config> conf, std::unique_ptr<llvm::Module> mod,
		std::unique_ptr<ModuleInfo> MI, GenMCDriver::Mode mode /* = GenMCDriver::VerificationMode{} */)
	: GenMCDriver(conf, std::move(mod), std::move(MI), mode) {}

bool RISCVDriver::isDepTracking() const
{
	return 1;
}

bool RISCVDriver::visitCalc89_0(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();




	return true;
}

bool RISCVDriver::visitCalc89_1(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = lab; true)if (calcRes.update(pLab->view(0)); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc89_2(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_2[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_2[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_2(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	visitedCalc89_2[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_3(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedCalc89_6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_6(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedCalc89_6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_6(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_7(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_7(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_5(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_2(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_4(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_4(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_4(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_4(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_11(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_12(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_12(pLab, calcRes)){
				return false;
		}
		
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_10(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_10(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_10(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedCalc89_10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_10(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_9(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_9(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_9(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}

	return true;
}

bool RISCVDriver::visitCalc89_4(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc89_5(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_5[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_5[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_5(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_12(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_12(pLab, calcRes)){
				return false;
		}
		
	}

	visitedCalc89_5[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_6(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_6[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_6[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_6(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	visitedCalc89_6[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_7(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_7[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_7[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedCalc89_6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_6(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_7(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}

	visitedCalc89_7[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_8(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc89_8(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc89_9(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_9[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_9[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_9(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}

	visitedCalc89_9[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_10(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();

	if (visitedCalc89_10[lab->getStamp().get()] != NodeStatus::unseen)
		return true;
	visitedCalc89_10[lab->getStamp().get()] = NodeStatus::entered;

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCalc89_4(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedCalc89_10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen) {
			if (!visitCalc89_10(pLab, calcRes)){
				return false;
		}
		
		} else if (status == NodeStatus::entered) {

		} else if (status == NodeStatus::left) {

		}
	}

	visitedCalc89_10[lab->getStamp().get()] = NodeStatus::left;
	return true;
}

bool RISCVDriver::visitCalc89_11(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc89_12(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCalc89_1(pLab, calcRes)){
				return false;
		}
		
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc89_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

View RISCVDriver::visitCalc89(const EventLabel *lab) const
{
	auto &g = getGraph();
	View calcRes;

	visitedCalc89_2.clear();
	visitedCalc89_2.resize(g.getMaxStamp().get() + 1);
	visitedCalc89_5.clear();
	visitedCalc89_5.resize(g.getMaxStamp().get() + 1);
	visitedCalc89_6.clear();
	visitedCalc89_6.resize(g.getMaxStamp().get() + 1);
	visitedCalc89_7.clear();
	visitedCalc89_7.resize(g.getMaxStamp().get() + 1);
	visitedCalc89_9.clear();
	visitedCalc89_9.resize(g.getMaxStamp().get() + 1);
	visitedCalc89_10.clear();
	visitedCalc89_10.resize(g.getMaxStamp().get() + 1);

	visitCalc89_3(lab, calcRes);
	visitCalc89_8(lab, calcRes);
	return calcRes;
}
auto RISCVDriver::checkCalc89(const EventLabel *lab) const
{
	auto &g = getGraph();

	return visitCalc89(lab);
}
bool RISCVDriver::visitCalc94_0(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();




	return true;
}

bool RISCVDriver::visitCalc94_1(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = lab; true)if (calcRes.update(pLab->view(1)); true) {
			if (!visitCalc94_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc94_2(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc94_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCalc94_3(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc94_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitCalc94_3(const EventLabel *lab, View &calcRes) const 
{
	auto &g = getGraph();


	if (auto pLab = tc_pred(g, lab); pLab) {
			if (!visitCalc94_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = tj_pred(g, lab); pLab) {
			if (!visitCalc94_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = rf_pred(g, lab); pLab) {
			if (!visitCalc94_1(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = tc_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc94_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = tj_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc94_0(pLab, calcRes)){
				return false;
		}
		
	}
	if (auto pLab = rf_pred(g, lab); pLab)if (calcRes.updateIdx(pLab->getPos()); true) {
			if (!visitCalc94_0(pLab, calcRes)){
				return false;
		}
		
	}

	return true;
}

View RISCVDriver::visitCalc94(const EventLabel *lab) const
{
	auto &g = getGraph();
	View calcRes;


	visitCalc94_2(lab, calcRes);
	return calcRes;
}
auto RISCVDriver::checkCalc94(const EventLabel *lab) const
{
	auto &g = getGraph();

	return visitCalc94(lab);
}
void RISCVDriver::calculateSaved(EventLabel *lab)
{
}

void RISCVDriver::calculateViews(EventLabel *lab)
{
	lab->addView(checkCalc89(lab));
	lab->addView(checkCalc94(lab));
}

void RISCVDriver::updateMMViews(EventLabel *lab)
{
	calculateViews(lab);
	calculateSaved(lab);
}

const View &RISCVDriver::getHbView(const EventLabel *lab) const
{
	return lab->view(0);
}


bool RISCVDriver::isWriteRfBefore(Event a, Event b)
{
	auto &g = getGraph();
	auto &before = g.getEventLabel(b)->view(0);
	if (before.contains(a))
		return true;

	const EventLabel *lab = g.getEventLabel(a);

	BUG_ON(!llvm::isa<WriteLabel>(lab));
	auto *wLab = static_cast<const WriteLabel *>(lab);
	for (auto &rLab : wLab->readers())
		if (before.contains(rLab.getPos()))
			return true;
	return false;
}

std::vector<Event>
RISCVDriver::getInitRfsAtLoc(SAddr addr)
{
	std::vector<Event> result;

	for (const auto &lab : getGraph().labels()) {
		if (auto *rLab = llvm::dyn_cast<ReadLabel>(&lab))
			if (rLab->getRf()->getPos().isInitializer() && rLab->getAddr() == addr)
				result.push_back(rLab->getPos());
	}
	return result;
}

bool RISCVDriver::isHbOptRfBefore(const Event e, const Event write)
{
	auto &g = getGraph();
	const EventLabel *lab = g.getEventLabel(write);

	BUG_ON(!llvm::isa<WriteLabel>(lab));
	auto *sLab = static_cast<const WriteLabel *>(lab);
	if (sLab->view(0).contains(e))
		return true;

	for (auto &rLab : sLab->readers()) {
		if (rLab.view(0).contains(e))
			return true;
	}
	return false;
}

ExecutionGraph::co_iterator
RISCVDriver::splitLocMOBefore(SAddr addr, Event e)
{
	auto &g = getGraph();
	auto rit = std::find_if(g.co_rbegin(addr), g.co_rend(addr), [&](auto &lab){
		return isWriteRfBefore(lab.getPos(), e);
	});
	/* Convert to forward iterator, but be _really_ careful */
	if (rit == g.co_rend(addr))
		return g.co_begin(addr);
	return ++ExecutionGraph::co_iterator(*rit);
}

ExecutionGraph::co_iterator
RISCVDriver::splitLocMOAfterHb(SAddr addr, const Event read)
{
	auto &g = getGraph();

	auto initRfs = g.getInitRfsAtLoc(addr);
	if (std::any_of(initRfs.begin(), initRfs.end(), [&read,&g](const Event &rf){
		return g.getEventLabel(rf)->view(0).contains(read);
	}))
		return g.co_begin(addr);

	auto it = std::find_if(g.co_begin(addr), g.co_end(addr), [&](auto &lab){
		return isHbOptRfBefore(read, lab.getPos());
	});
	if (it == g.co_end(addr) || it->view(0).contains(read))
		return it;
	return ++it;
}

ExecutionGraph::co_iterator
RISCVDriver::splitLocMOAfter(SAddr addr, const Event e)
{
	auto &g = getGraph();
	return std::find_if(g.co_begin(addr), g.co_end(addr), [&](auto &lab){
		return isHbOptRfBefore(e, lab.getPos());
	});
}

std::vector<Event>
RISCVDriver::getCoherentStores(SAddr addr, Event read)
{
	auto &g = getGraph();
	std::vector<Event> stores;

	/* Fastpath: co_max(G) is po-before R */
	auto comax = g.co_rbegin(addr) == g.co_rend(addr) ? Event::getInit() :
		     g.co_rbegin(addr)->getPos();
	if (comax.thread == read.thread && comax.index < read.index)
		return {comax};

	/*
	 * If there are no stores (rf?;hb)-before the current event
	 * then we can read read from all concurrent stores and the
	 * initializer store. Otherwise, we can read from all concurrent
	 * stores and the mo-latest of the (rf?;hb)-before stores.
	 */
	auto begIt = splitLocMOBefore(addr, read);
	if (begIt == g.co_begin(addr))
		stores.push_back(Event::getInit());
	else {
		stores.push_back((--begIt)->getPos());
		++begIt;
	}

	/*
	 * If the model supports out-of-order execution we have to also
	 * account for the possibility the read is hb-before some other
	 * store, or some read that reads from a store.
	 */
	auto endIt = (isDepTracking()) ? splitLocMOAfterHb(addr, read) : g.co_end(addr);
	std::transform(begIt, endIt, std::back_inserter(stores), [&](auto &lab){
		return lab.getPos();
	});
	return stores;
}

std::vector<Event>
RISCVDriver::getMOOptRfAfter(const WriteLabel *sLab)
{
	std::vector<Event> after;
	std::vector<const ReadLabel *> rfAfter;

	const auto &g = getGraph();
	std::for_each(g.co_succ_begin(sLab), g.co_succ_end(sLab),
		      [&](auto &wLab){
			      after.push_back(wLab.getPos());
			      std::transform(wLab.readers_begin(), wLab.readers_end(), std::back_inserter(rfAfter),
			      [&](auto &rLab){ return &rLab; });
	});
	std::transform(rfAfter.begin(), rfAfter.end(), std::back_inserter(after), [](auto *rLab){
		return rLab->getPos();
	});
	return after;
}

std::vector<Event>
RISCVDriver::getMOInvOptRfAfter(const WriteLabel *sLab)
{
	auto &g = getGraph();
	std::vector<Event> after;
	std::vector<const ReadLabel *> rfAfter;

	/* First, add (mo;rf?)-before */
	std::for_each(g.co_pred_begin(sLab),
		      g.co_pred_end(sLab), [&](auto &wLab){
			      after.push_back(wLab.getPos());
			      std::transform(wLab.readers_begin(), wLab.readers_end(), std::back_inserter(rfAfter),
			      [&](auto &rLab){ return &rLab; });
	});
	std::transform(rfAfter.begin(), rfAfter.end(), std::back_inserter(after), [](auto *rLab){
		return rLab->getPos();
	});

	/* Then, we add the reader list for the initializer */
	auto initRfs = g.getInitRfsAtLoc(sLab->getAddr());
	after.insert(after.end(), initRfs.begin(), initRfs.end());
	return after;
}

static std::vector<Event>
getRevisitableFrom(const ExecutionGraph &g, const WriteLabel *sLab,
		   const VectorClock &pporf, const WriteLabel *coPred)
{
	auto pendingRMW = g.getPendingRMW(sLab);
	std::vector<Event> loads;

	for (auto &rLab : coPred->readers()) {
		if (!pporf.contains(rLab.getPos()) && rLab.getAddr() == sLab->getAddr() &&
		    rLab.isRevisitable() && rLab.wasAddedMax())
			loads.push_back(rLab.getPos());
	}
	if (!pendingRMW.isInitializer())
		loads.erase(std::remove_if(loads.begin(), loads.end(),
					   [&](Event &e) {
						   auto *confLab = g.getEventLabel(pendingRMW);
						   return g.getEventLabel(e)->getStamp() >
							  confLab->getStamp();
					   }),
			    loads.end());
	return loads;
}

std::vector<Event>
RISCVDriver::getCoherentRevisits(const WriteLabel *sLab, const VectorClock &pporf)
{
	auto &g = getGraph();
	std::vector<Event> ls;

	/* Fastpath: previous co-max is ppo-before SLAB */
	auto prevCoMaxIt = std::find_if(g.co_rbegin(sLab->getAddr()), g.co_rend(sLab->getAddr()),
					[&](auto &lab) { return lab.getPos() != sLab->getPos(); });
	if (prevCoMaxIt != g.co_rend(sLab->getAddr()) && pporf.contains(prevCoMaxIt->getPos())) {
		ls = getRevisitableFrom(g, sLab, pporf, &*prevCoMaxIt);
	} else {
		ls = g.getRevisitable(sLab, pporf);
	}

	/* If this store is po- and mo-maximal then we are done */
	if (!isDepTracking() && g.isCoMaximal(sLab->getAddr(), sLab->getPos()))
		return ls;

	/* First, we have to exclude (mo;rf?;hb?;sb)-after reads */
	auto optRfs = getMOOptRfAfter(sLab);
	ls.erase(std::remove_if(ls.begin(), ls.end(), [&](Event e)
				{ const View &before = g.getEventLabel(e)->view(0);
				  return std::any_of(optRfs.begin(), optRfs.end(),
					 [&](Event ev)
					 { return before.contains(ev); });
				}), ls.end());

	/* If out-of-order event addition is not supported, then we are done
	 * due to po-maximality */
	if (!isDepTracking())
		return ls;

	/* Otherwise, we also have to exclude hb-before loads */
	ls.erase(std::remove_if(ls.begin(), ls.end(), [&](Event e)
		{ return g.getEventLabel(sLab->getPos())->view(0).contains(e); }),
		ls.end());

	/* ...and also exclude (mo^-1; rf?; (hb^-1)?; sb^-1)-after reads in
	 * the resulting graph */
	auto &before = pporf;
	auto moInvOptRfs = getMOInvOptRfAfter(sLab);
	ls.erase(std::remove_if(ls.begin(), ls.end(), [&](Event e)
				{ auto *eLab = g.getEventLabel(e);
				  auto v = g.getViewFromStamp(eLab->getStamp());
				  v->update(before);
				  return std::any_of(moInvOptRfs.begin(),
						     moInvOptRfs.end(),
						     [&](Event ev)
						     { return v->contains(ev) &&
						       g.getEventLabel(ev)->view(0).contains(e); });
				}),
		 ls.end());

	return ls;
}

std::vector<Event>
RISCVDriver::getCoherentPlacings(SAddr addr, Event store, bool isRMW)
{
	auto &g = getGraph();
	std::vector<Event> result;

	/* If it is an RMW store, there is only one possible position in MO */
	if (isRMW) {
		auto *rLab = llvm::dyn_cast<ReadLabel>(g.getEventLabel(store.prev()));
		BUG_ON(!rLab);
		auto *rfLab = rLab->getRf();
		BUG_ON(!rfLab);
		result.push_back(rfLab->getPos());
		return result;
	}

	/* Otherwise, we calculate the full range and add the store */
	auto rangeBegin = splitLocMOBefore(addr, store);
	auto rangeEnd = (isDepTracking()) ? splitLocMOAfter(addr, store) : g.co_end(addr);
	auto cos = llvm::iterator_range(rangeBegin, rangeEnd) |
		   std::views::filter([&](auto &sLab) { return !g.isRMWStore(sLab.getPos()); }) |
		   std::views::transform([&](auto &sLab) {
			   auto *pLab = g.co_imm_pred(&sLab);
			   return pLab ? pLab->getPos() : Event::getInit();
		   });
	std::ranges::copy(cos, std::back_inserter(result));
	result.push_back(rangeEnd == g.co_end(addr)   ? g.co_max(addr)->getPos()
			 : !g.co_imm_pred(&*rangeEnd) ? Event::getInit()
						      : g.co_imm_pred(&*rangeEnd)->getPos());
	return result;
}
bool RISCVDriver::visitCoherence_0(const EventLabel *lab) const 
{
	auto &g = getGraph();

	++visitedCoherenceAccepting;


	--visitedCoherenceAccepting;
	return true;
}

bool RISCVDriver::visitCoherence_1(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_1[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_8[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_8(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_8[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_8(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_8[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_8(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_4[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_4(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedCoherence_11[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_11(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_11[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_11(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCoherence_10(pLab)){
				return false;
		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedCoherence_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedCoherence_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_1[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_2(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_2[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_2[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_3(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_3[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_3[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_4(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_4[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_4[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_4(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedCoherence_11[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_11(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_11[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_11(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_4[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_5(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_5[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_5[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_6(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_6[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedCoherence_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_6[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_7(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_7[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_7[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_8(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_8[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_8[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_8(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_8[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_9(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_9[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedCoherence_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedCoherence_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_9[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherence_10(const EventLabel *lab) const 
{
	auto &g = getGraph();


	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	return true;
}

bool RISCVDriver::visitCoherence_11(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedCoherence_11[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::entered };

	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitCoherence_0(pLab)){
				return false;
		}
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedCoherence_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitCoherence_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedCoherenceAccepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedCoherence_11[lab->getStamp().get()] = { visitedCoherenceAccepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitCoherenceFull() const
{
	auto &g = getGraph();

	visitedCoherenceAccepting = 0;
	visitedCoherence_1.clear();
	visitedCoherence_1.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_2.clear();
	visitedCoherence_2.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_3.clear();
	visitedCoherence_3.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_4.clear();
	visitedCoherence_4.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_5.clear();
	visitedCoherence_5.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_6.clear();
	visitedCoherence_6.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_7.clear();
	visitedCoherence_7.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_8.clear();
	visitedCoherence_8.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_9.clear();
	visitedCoherence_9.resize(g.getMaxStamp().get() + 1);
	visitedCoherence_11.clear();
	visitedCoherence_11.resize(g.getMaxStamp().get() + 1);
	return true
		&& std::ranges::all_of(g.labels(), [&](auto &lab){ return visitedCoherence_1[lab.getStamp().get()].status != NodeStatus::unseen || visitCoherence_1(&lab); });
}

bool RISCVDriver::visitConsAcyclic1_0(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_0[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_0[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_0(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_0[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_1(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_1[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_1[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_2(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_2[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_2[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_3(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_3[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_3[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_4(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_4[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_4[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_4(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_4[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_5(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_5[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_5[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_6(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_6[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_6[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_7(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_7[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_7[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_8(const EventLabel *lab) const 
{
	auto &g = getGraph();


	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	return true;
}

bool RISCVDriver::visitConsAcyclic1_9(const EventLabel *lab) const 
{
	auto &g = getGraph();

	visitedConsAcyclic1_9[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };

	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	visitedConsAcyclic1_9[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1_10(const EventLabel *lab) const 
{
	auto &g = getGraph();

	++visitedConsAcyclic1Accepting;
	visitedConsAcyclic1_10[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::entered };


	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_2[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_2(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_4[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_4(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_1[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_1(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_0[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_0(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_7[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_7(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_5[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_5(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_3[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_3(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = co_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	for (auto &tmp : fr_imm_preds(g, lab)) if (auto *pLab = &tmp; true) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = rfe_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto &node = visitedConsAcyclic1_10[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_10(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 1)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto &node = visitedConsAcyclic1_6[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_6(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto &node = visitedConsAcyclic1_9[pLab->getStamp().get()];
		if (node.status == NodeStatus::unseen) {
			if (!visitConsAcyclic1_9(pLab)){
				return false;
		}
		} else if (node.status == NodeStatus::entered && (visitedConsAcyclic1Accepting > node.count || 0)) {

			return false;
		} else if (node.status == NodeStatus::left) {

		}
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitConsAcyclic1_8(pLab)){
				return false;
		}
	}
	--visitedConsAcyclic1Accepting;
	visitedConsAcyclic1_10[lab->getStamp().get()] = { visitedConsAcyclic1Accepting, NodeStatus::left };
	return true;
}

bool RISCVDriver::visitConsAcyclic1(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedConsAcyclic1Accepting = 0;
	visitedConsAcyclic1_0.clear();
	visitedConsAcyclic1_0.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_1.clear();
	visitedConsAcyclic1_1.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_2.clear();
	visitedConsAcyclic1_2.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_3.clear();
	visitedConsAcyclic1_3.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_4.clear();
	visitedConsAcyclic1_4.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_5.clear();
	visitedConsAcyclic1_5.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_6.clear();
	visitedConsAcyclic1_6.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_7.clear();
	visitedConsAcyclic1_7.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_9.clear();
	visitedConsAcyclic1_9.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_10.clear();
	visitedConsAcyclic1_10.resize(g.getMaxStamp().get() + 1);
	return true
		&& (visitedConsAcyclic1_10[lab->getStamp().get()].status != NodeStatus::unseen || visitConsAcyclic1_10(lab));
}

bool RISCVDriver::visitConsAcyclic1Full() const
{
	auto &g = getGraph();

	visitedConsAcyclic1Accepting = 0;
	visitedConsAcyclic1_0.clear();
	visitedConsAcyclic1_0.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_1.clear();
	visitedConsAcyclic1_1.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_2.clear();
	visitedConsAcyclic1_2.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_3.clear();
	visitedConsAcyclic1_3.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_4.clear();
	visitedConsAcyclic1_4.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_5.clear();
	visitedConsAcyclic1_5.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_6.clear();
	visitedConsAcyclic1_6.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_7.clear();
	visitedConsAcyclic1_7.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_9.clear();
	visitedConsAcyclic1_9.resize(g.getMaxStamp().get() + 1);
	visitedConsAcyclic1_10.clear();
	visitedConsAcyclic1_10.resize(g.getMaxStamp().get() + 1);
	return true
		&& std::ranges::all_of(g.labels(), [&](auto &lab){ return visitedConsAcyclic1_10[lab.getStamp().get()].status != NodeStatus::unseen || visitConsAcyclic1_10(&lab); });
}

bool RISCVDriver::checkConsAcyclic1(const EventLabel *lab) const
{
	auto &g = getGraph();


	return visitConsAcyclic1(lab);
}
bool RISCVDriver::visitError2(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError2_0(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (!v.contains(lab->getPos())) {
cexLab = lab;
		return false;
	}


	return true;
}

bool RISCVDriver::visitLHSUnlessError2_1(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (auto pLab = alloc_pred(g, lab); pLab) {
			if (!visitLHSUnlessError2_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError2(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError2Accepting.clear();
 	visitedLHSUnlessError2Accepting.resize(g.getMaxStamp().get() + 1, false);
	auto &v = lab->view(0);

	return true
		&& visitLHSUnlessError2_1(lab, v);
}

bool RISCVDriver::checkError2(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError2(lab))
		return true;

	return visitError2(lab);
}
bool RISCVDriver::visitError3(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError3_0(const EventLabel *lab) const 
{
	auto &g = getGraph();


	return false;


	return true;
}

bool RISCVDriver::visitLHSUnlessError3_1(const EventLabel *lab) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<FreeLabel>(lab) && !llvm::isa<HpRetireLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<FreeLabel>(pLab) && !llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError3_0(pLab)){
			return false;
		}
		
	}
	if (true && llvm::isa<FreeLabel>(lab) && !llvm::isa<HpRetireLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError3_0(pLab)){
			return false;
		}
		
	}
	if (true && llvm::isa<HpRetireLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<FreeLabel>(pLab) && !llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError3_0(pLab)){
			return false;
		}
		
	}
	if (true && llvm::isa<HpRetireLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError3_0(pLab)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError3(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError3Accepting.clear();
 	visitedLHSUnlessError3Accepting.resize(g.getMaxStamp().get() + 1, false);
	visitedRHSUnlessError3Accepting.clear();
	visitedRHSUnlessError3Accepting.resize(g.getMaxStamp().get() + 1, false);

	if (!visitLHSUnlessError3_1(lab))
		return false;
	for (auto i = 0u; i < visitedLHSUnlessError3Accepting.size(); i++) {
		if (visitedLHSUnlessError3Accepting[i] && !visitedRHSUnlessError3Accepting[i]) {
			cexLab = &*std::find_if(g.label_begin(), g.label_end(), [&](auto &lab){ return lab.getStamp() == i; });
			return false;
		}
	}
	return true;
}

bool RISCVDriver::checkError3(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError3(lab))
		return true;

	return visitError3(lab);
}
bool RISCVDriver::visitError4(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError4_0(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (!v.contains(lab->getPos())) {
cexLab = lab;
		return false;
	}


	return true;
}

bool RISCVDriver::visitLHSUnlessError4_1(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	for (auto &tmp : alloc_succs(g, lab)) if (auto *pLab = &tmp; true) {
			if (!visitLHSUnlessError4_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitLHSUnlessError4_2(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<FreeLabel>(lab) && !llvm::isa<HpRetireLabel>(lab))if (auto pLab = free_pred(g, lab); pLab) {
			if (!visitLHSUnlessError4_1(pLab, v)){
			return false;
		}
		
	}
	if (true && llvm::isa<FreeLabel>(lab) && !llvm::isa<HpRetireLabel>(lab))if (auto pLab = free_pred(g, lab); pLab) {
			if (!visitLHSUnlessError4_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError4(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError4Accepting.clear();
 	visitedLHSUnlessError4Accepting.resize(g.getMaxStamp().get() + 1, false);
	auto &v = lab->view(0);

	return true
		&& visitLHSUnlessError4_2(lab, v);
}

bool RISCVDriver::checkError4(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError4(lab))
		return true;

	return visitError4(lab);
}
bool RISCVDriver::visitError5(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError5_0(const EventLabel *lab) const 
{
	auto &g = getGraph();


	return false;


	return true;
}

bool RISCVDriver::visitLHSUnlessError5_1(const EventLabel *lab) const 
{
	auto &g = getGraph();


	if (auto pLab = free_succ(g, lab); pLab)if (true && llvm::isa<FreeLabel>(pLab) && !llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError5_0(pLab)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitLHSUnlessError5_2(const EventLabel *lab) const 
{
	auto &g = getGraph();


	if (auto pLab = alloc_pred(g, lab); pLab) {
			if (!visitLHSUnlessError5_1(pLab)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError5(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError5Accepting.clear();
 	visitedLHSUnlessError5Accepting.resize(g.getMaxStamp().get() + 1, false);
	visitedRHSUnlessError5Accepting.clear();
	visitedRHSUnlessError5Accepting.resize(g.getMaxStamp().get() + 1, false);

	if (!visitLHSUnlessError5_2(lab))
		return false;
	for (auto i = 0u; i < visitedLHSUnlessError5Accepting.size(); i++) {
		if (visitedLHSUnlessError5Accepting[i] && !visitedRHSUnlessError5Accepting[i]) {
			cexLab = &*std::find_if(g.label_begin(), g.label_end(), [&](auto &lab){ return lab.getStamp() == i; });
			return false;
		}
	}
	return true;
}

bool RISCVDriver::checkError5(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError5(lab))
		return true;

	return visitError5(lab);
}
bool RISCVDriver::visitError6(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError6_0(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (!v.contains(lab->getPos())) {
cexLab = lab;
		return false;
	}


	return true;
}

bool RISCVDriver::visitLHSUnlessError6_1(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	for (auto &tmp : alloc_succs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<MemAccessLabel>(pLab) && llvm::dyn_cast<MemAccessLabel>(pLab)->getAddr().isDynamic() && !isHazptrProtected(llvm::dyn_cast<MemAccessLabel>(pLab))) {
			if (!visitLHSUnlessError6_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitLHSUnlessError6_2(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<HpRetireLabel>(lab))if (auto pLab = free_pred(g, lab); pLab) {
			if (!visitLHSUnlessError6_1(pLab, v)){
			return false;
		}
		
	}
	if (true && llvm::isa<HpRetireLabel>(lab))if (auto pLab = free_pred(g, lab); pLab)if (true && llvm::isa<MemAccessLabel>(pLab) && llvm::dyn_cast<MemAccessLabel>(pLab)->getAddr().isDynamic() && !isHazptrProtected(llvm::dyn_cast<MemAccessLabel>(pLab))) {
			if (!visitLHSUnlessError6_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError6(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError6Accepting.clear();
 	visitedLHSUnlessError6Accepting.resize(g.getMaxStamp().get() + 1, false);
	auto &v = lab->view(0);

	return true
		&& visitLHSUnlessError6_2(lab, v);
}

bool RISCVDriver::checkError6(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError6(lab))
		return true;

	return visitError6(lab);
}
bool RISCVDriver::visitError7(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError7_0(const EventLabel *lab) const 
{
	auto &g = getGraph();


	return false;


	return true;
}

bool RISCVDriver::visitLHSUnlessError7_1(const EventLabel *lab) const 
{
	auto &g = getGraph();


	if (auto pLab = free_succ(g, lab); pLab)if (true && llvm::isa<HpRetireLabel>(pLab)) {
			if (!visitLHSUnlessError7_0(pLab)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitLHSUnlessError7_2(const EventLabel *lab) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<MemAccessLabel>(lab) && llvm::dyn_cast<MemAccessLabel>(lab)->getAddr().isDynamic() && !isHazptrProtected(llvm::dyn_cast<MemAccessLabel>(lab)))if (auto pLab = alloc_pred(g, lab); pLab) {
			if (!visitLHSUnlessError7_1(pLab)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError7(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError7Accepting.clear();
 	visitedLHSUnlessError7Accepting.resize(g.getMaxStamp().get() + 1, false);
	visitedRHSUnlessError7Accepting.clear();
	visitedRHSUnlessError7Accepting.resize(g.getMaxStamp().get() + 1, false);

	if (!visitLHSUnlessError7_2(lab))
		return false;
	for (auto i = 0u; i < visitedLHSUnlessError7Accepting.size(); i++) {
		if (visitedLHSUnlessError7Accepting[i] && !visitedRHSUnlessError7Accepting[i]) {
			cexLab = &*std::find_if(g.label_begin(), g.label_end(), [&](auto &lab){ return lab.getStamp() == i; });
			return false;
		}
	}
	return true;
}

bool RISCVDriver::checkError7(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError7(lab))
		return true;

	return visitError7(lab);
}
bool RISCVDriver::visitError8(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessError8_0(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (!v.contains(lab->getPos())) {
cexLab = lab;
		return false;
	}


	return true;
}

bool RISCVDriver::visitLHSUnlessError8_1(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (true && lab->isNotAtomic() && llvm::isa<WriteLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}
	if (true && lab->isNotAtomic() && llvm::isa<WriteLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<ReadLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}
	if (true && lab->isNotAtomic() && llvm::isa<ReadLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && pLab->isNotAtomic() && llvm::isa<WriteLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && pLab->isNotAtomic() && llvm::isa<ReadLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}
	if (true && llvm::isa<ReadLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && pLab->isNotAtomic() && llvm::isa<WriteLabel>(pLab)) {
			if (!visitLHSUnlessError8_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessError8(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessError8Accepting.clear();
 	visitedLHSUnlessError8Accepting.resize(g.getMaxStamp().get() + 1, false);
	auto &v = lab->view(0);

	return true
		&& visitLHSUnlessError8_1(lab, v);
}

bool RISCVDriver::checkError8(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessError8(lab))
		return true;

	return visitError8(lab);
}
bool RISCVDriver::visitWarning9(const EventLabel *lab) const
{
	return false;
}

bool RISCVDriver::visitLHSUnlessWarning9_0(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (!v.contains(lab->getPos())) {
cexLab = lab;
		return false;
	}


	return true;
}

bool RISCVDriver::visitLHSUnlessWarning9_1(const EventLabel *lab, const View &v) const 
{
	auto &g = getGraph();


	if (true && llvm::isa<WriteLabel>(lab))for (auto &tmp : samelocs(g, lab)) if (auto *pLab = &tmp; true)if (true && llvm::isa<WriteLabel>(pLab)) {
			if (!visitLHSUnlessWarning9_0(pLab, v)){
			return false;
		}
		
	}

	return true;
}

bool RISCVDriver::visitUnlessWarning9(const EventLabel *lab) const
{
	auto &g = getGraph();

	visitedLHSUnlessWarning9Accepting.clear();
 	visitedLHSUnlessWarning9Accepting.resize(g.getMaxStamp().get() + 1, false);
	auto &v = lab->view(1);

	return true
		&& visitLHSUnlessWarning9_1(lab, v);
}

bool RISCVDriver::checkWarning9(const EventLabel *lab) const
{
	auto &g = getGraph();


	if (visitUnlessWarning9(lab))
		return true;

	return visitWarning9(lab);
}
VerificationError RISCVDriver::checkErrors(const EventLabel *lab, const EventLabel *&race) const
{
	if (!checkError2(lab)) {
		race = cexLab;
		return VerificationError::VE_AccessNonMalloc;
	}

	if (!checkError3(lab)) {
		race = cexLab;
		return VerificationError::VE_DoubleFree;
	}

	if (!checkError4(lab)) {
		race = cexLab;
		return VerificationError::VE_AccessFreed;
	}

	if (!checkError5(lab)) {
		race = cexLab;
		return VerificationError::VE_AccessFreed;
	}

	if (!checkError6(lab)) {
		race = cexLab;
		return VerificationError::VE_AccessFreed;
	}

	if (!checkError7(lab)) {
		race = cexLab;
		return VerificationError::VE_AccessFreed;
	}

	if (!checkError8(lab)) {
		race = cexLab;
		return VerificationError::VE_RaceNotAtomic;
	}

	return VerificationError::VE_OK;
}

std::vector<VerificationError> RISCVDriver::checkWarnings(const EventLabel *lab, const VSet<VerificationError> &seenWarnings, std::vector<const EventLabel *> &racyLabs) const
{
	std::vector<VerificationError> result;

	if (seenWarnings.count(VerificationError::VE_WWRace) == 0 && !checkWarning9(lab)) {
		racyLabs.push_back(cexLab);
		result.push_back(VerificationError::VE_WWRace);
	}

	return result;
}

bool RISCVDriver::isConsistent(const EventLabel *lab) const
{

	return true
		&& checkConsAcyclic1(lab);
}

void RISCVDriver::visitPPoRf0(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf0[lab->getStamp().get()] = NodeStatus::entered;
	pporf.updateIdx(lab->getPos());
	visitedPPoRf0[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf1(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf1[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf1[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf1(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf1[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf2(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf2[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = tc_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = tj_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = rfe_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf3[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf3(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf8[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf8(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf8[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf8(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf8[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf8(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf8[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf8(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf6(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf6(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf4[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf4(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf4[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf4(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedPPoRf5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf5(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedPPoRf5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf5(pLab, pporf);
	}
	if (auto pLab = tc_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = tj_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = rfe_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf9(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf9(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf9(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf10(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf10(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf10(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf10(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf1[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf1(pLab, pporf);
	}
	if (auto pLab = tc_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = tj_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = rfe_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : ctrl_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab))if (auto pLab = poloc_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastAcquire())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isAtLeastRelease())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && lab->isSC())if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<WriteLabel>(lab) && ((llvm::isa<ReadLabel>(lab) && g.isRMWLoad(lab)) || (llvm::isa<WriteLabel>(lab) && g.isRMWStore(lab))))if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (true && llvm::isa<ReadLabel>(lab))if (auto pLab = rfi_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf11[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf11(pLab, pporf);
	}
	visitedPPoRf2[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf3(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf3[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf3[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf3(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf4[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf4(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf4[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf4(pLab, pporf);
	}
	visitedPPoRf3[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf4(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf4[lab->getStamp().get()] = NodeStatus::entered;
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf4[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf5(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf5[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf5(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf5[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf6(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf6[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf6[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf6(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<FenceLabel>(pLab)) {
		auto status = visitedPPoRf5[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf5(pLab, pporf);
	}
	visitedPPoRf6[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf7(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf7[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC())if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastAcquire()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isAtLeastRelease()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && pLab->isSC()) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf7[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf8(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf8[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab))))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<ReadLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf8[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf9(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf9[lab->getStamp().get()] = NodeStatus::entered;
	pporf.updateIdx(lab->getPos());
	if (auto pLab = po_imm_pred(g, lab); pLab)if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf9[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf9(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf9[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf10(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf10[lab->getStamp().get()] = NodeStatus::entered;
	if (auto pLab = po_imm_pred(g, lab); pLab)if (true && llvm::isa<WriteLabel>(pLab) && ((llvm::isa<ReadLabel>(pLab) && g.isRMWLoad(pLab)) || (llvm::isa<WriteLabel>(pLab) && g.isRMWStore(pLab)))) {
		auto status = visitedPPoRf8[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf8(pLab, pporf);
	}
	if (auto pLab = po_imm_pred(g, lab); pLab) {
		auto status = visitedPPoRf10[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf10(pLab, pporf);
	}
	visitedPPoRf10[lab->getStamp().get()] = NodeStatus::left;
}

void RISCVDriver::visitPPoRf11(const EventLabel *lab, DepView &pporf) const
{
	auto &g = getGraph();

	visitedPPoRf11[lab->getStamp().get()] = NodeStatus::entered;
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab))if (pporf.updateIdx(pLab->getPos()); true) {
		auto status = visitedPPoRf0[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf0(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf7[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf7(pLab, pporf);
	}
	for (auto &p : addr_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	for (auto &p : data_preds(g, lab)) if (auto *pLab = g.getEventLabel(p); true)if (true && llvm::isa<ReadLabel>(pLab)) {
		auto status = visitedPPoRf2[pLab->getStamp().get()];
		if (status == NodeStatus::unseen)
			visitPPoRf2(pLab, pporf);
	}
	visitedPPoRf11[lab->getStamp().get()] = NodeStatus::left;
}

DepView RISCVDriver::calcPPoRfBefore(const EventLabel *lab) const
{
	auto &g = getGraph();
	DepView pporf;
	pporf.updateIdx(lab->getPos());
	visitedPPoRf0.clear();
	visitedPPoRf0.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf1.clear();
	visitedPPoRf1.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf2.clear();
	visitedPPoRf2.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf3.clear();
	visitedPPoRf3.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf4.clear();
	visitedPPoRf4.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf5.clear();
	visitedPPoRf5.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf6.clear();
	visitedPPoRf6.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf7.clear();
	visitedPPoRf7.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf8.clear();
	visitedPPoRf8.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf9.clear();
	visitedPPoRf9.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf10.clear();
	visitedPPoRf10.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);
	visitedPPoRf11.clear();
	visitedPPoRf11.resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);

	visitPPoRf2(lab, pporf);
	visitPPoRf7(lab, pporf);
	return pporf;
}
std::unique_ptr<VectorClock> RISCVDriver::calculatePrefixView(const EventLabel *lab) const
{
	return std::make_unique<DepView>(calcPPoRfBefore(lab));
}

