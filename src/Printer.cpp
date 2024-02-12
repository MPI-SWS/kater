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

#include "Printer.hpp"

#include "Config.hpp"
#include "Error.hpp"
#include "KatModule.hpp"
#include "Utils.hpp"

#include <sstream>

std::vector<unsigned> Printer::calcToIdxMap;

const char *genmcCopyright =
	R"(/*
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
)";

const char *katerNotice =
	R"(/*******************************************************************************
 * CAUTION: This file is generated automatically by Kater -- DO NOT EDIT.
 *******************************************************************************/
)";

const char *coherenceFuns =
	R"(
bool #CLASS#::isWriteRfBefore(Event a, Event b)
{
	auto &g = getGraph();
	auto &before = g.getEventLabel(b)->view(#HB#);
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
#CLASS#::getInitRfsAtLoc(SAddr addr)
{
	std::vector<Event> result;

	for (const auto &lab : labels(getGraph())) {
		if (auto *rLab = llvm::dyn_cast<ReadLabel>(&lab))
			if (rLab->getRf()->getPos().isInitializer() && rLab->getAddr() == addr)
				result.push_back(rLab->getPos());
	}
	return result;
}

bool #CLASS#::isHbOptRfBefore(const Event e, const Event write)
{
	auto &g = getGraph();
	const EventLabel *lab = g.getEventLabel(write);

	BUG_ON(!llvm::isa<WriteLabel>(lab));
	auto *sLab = static_cast<const WriteLabel *>(lab);
	if (sLab->view(#HB#).contains(e))
		return true;

	for (auto &rLab : sLab->readers()) {
		if (rLab.view(#HB#).contains(e))
			return true;
	}
	return false;
}

ExecutionGraph::co_iterator
#CLASS#::splitLocMOBefore(SAddr addr, Event e)
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
#CLASS#::splitLocMOAfterHb(SAddr addr, const Event read)
{
	auto &g = getGraph();

	auto initRfs = g.getInitRfsAtLoc(addr);
	if (std::any_of(initRfs.begin(), initRfs.end(), [&read,&g](const Event &rf){
		return g.getEventLabel(rf)->view(#HB#).contains(read);
	}))
		return g.co_begin(addr);

	auto it = std::find_if(g.co_begin(addr), g.co_end(addr), [&](auto &lab){
		return isHbOptRfBefore(read, lab.getPos());
	});
	if (it == g.co_end(addr) || it->view(#HB#).contains(read))
		return it;
	return ++it;
}

ExecutionGraph::co_iterator
#CLASS#::splitLocMOAfter(SAddr addr, const Event e)
{
	auto &g = getGraph();
	return std::find_if(g.co_begin(addr), g.co_end(addr), [&](auto &lab){
		return isHbOptRfBefore(e, lab.getPos());
	});
}

std::vector<Event>
#CLASS#::getCoherentStores(SAddr addr, Event read)
{
	auto &g = getGraph();
	std::vector<Event> stores;

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
#CLASS#::getMOOptRfAfter(const WriteLabel *sLab)
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
#CLASS#::getMOInvOptRfAfter(const WriteLabel *sLab)
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

std::vector<Event>
#CLASS#::getCoherentRevisits(const WriteLabel *sLab, const VectorClock &pporf)
{
	auto &g = getGraph();
	auto ls = g.getRevisitable(sLab, pporf);

	/* If this store is po- and mo-maximal then we are done */
	if (!isDepTracking() && g.isCoMaximal(sLab->getAddr(), sLab->getPos()))
		return ls;

	/* First, we have to exclude (mo;rf?;hb?;sb)-after reads */
	auto optRfs = getMOOptRfAfter(sLab);
	ls.erase(std::remove_if(ls.begin(), ls.end(), [&](Event e)
				{ const View &before = g.getEventLabel(e)->view(#HB#);
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
		{ return g.getEventLabel(sLab->getPos())->view(#HB#).contains(e); }),
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
						       g.getEventLabel(ev)->view(#HB#).contains(e); });
				}),
		 ls.end());

	return ls;
}

llvm::iterator_range<ExecutionGraph::co_iterator>
#CLASS#::getCoherentPlacings(SAddr addr, Event store, bool isRMW)
{
	auto &g = getGraph();

	/* If it is an RMW store, there is only one possible position in MO */
	if (isRMW) {
		if (auto *rLab = llvm::dyn_cast<ReadLabel>(g.getEventLabel(store.prev()))) {
			auto *rfLab = rLab->getRf();
			BUG_ON(!rfLab);
			if (auto *wLab = llvm::dyn_cast<WriteLabel>(rfLab)) {
				auto wIt = g.co_succ_begin(wLab);
				return llvm::iterator_range<ExecutionGraph::co_iterator>(wIt, wIt);
			}
			return llvm::iterator_range<ExecutionGraph::co_iterator>(g.co_begin(addr),
										 g.co_begin(addr));
		}
		BUG();
	}

	/* Otherwise, we calculate the full range and add the store */
	auto rangeBegin = splitLocMOBefore(addr, store);
	auto rangeEnd = (isDepTracking()) ? splitLocMOAfter(addr, store) : g.co_end(addr);
	return llvm::iterator_range(rangeBegin, rangeEnd);

})";

Printer::Printer(const KatModule &module, const std::string &dirPrefix,
		 const std::string &outPrefix)
	: module_(module)
{
	auto name = !outPrefix.empty() ? outPrefix : "Demo";

	/* Construct all the names to be used */
	std::transform(name.begin(), name.end(), std::back_inserter(prefix), ::toupper);
	className = prefix + "Driver";
	guardName = std::string("__") + prefix + "_DRIVER_HPP__";

	/* Open required streams */
	if (!outPrefix.empty()) {
		foutHpp = openFileForWriting(dirPrefix + "/" + className + ".hpp");
		outHpp = &foutHpp;
		foutCpp = openFileForWriting(dirPrefix + "/" + className + ".cpp");
		outCpp = &foutCpp;
	}
}

auto replaceAll(std::string &inout, const std::string &what, const std::string &with) -> std::size_t
{
	std::size_t count{};
	for (std::string::size_type pos{};
	     std::string::npos != (pos = inout.find(what.data(), pos, what.length()));
	     pos += with.length(), ++count) {
		inout.replace(pos, what.length(), with.data(), with.length());
	}
	return count;
}

void Printer::printPredSet(std::ostream &ostr, const std::string &arg, const PredicateSet &preds)
{
	if (preds.begin() == preds.end())
		return;

	ostr << "if (true";
	for (const auto &p : preds.preds()) {
		ostr << " && ";

		auto s = getModule().getTheory().getInfo(p).genmc;
		replaceAll(s, "#", arg);
		ostr << s;
	}
	ostr << ")";
}

void Printer::printRelation(std::ostream &ostr, const std::string &res, const std::string &arg,
			    const TransLabel *r)
{
	if (r->isPredicate()) {
		ostr << "if (auto " << res << " = " << arg << "; true)";
		return;
	}

	if (r->isBuiltin()) {
		const auto &outs = getModule().getTheory().getInfo(*r->getRelation()).genmc;
		const auto &s = r->getRelation()->isInverse() ? outs.pred : outs.succ;
		// if ((n.type == RelType::OneOne) || (flipped && n.type == RelType::ManyOne))
		// 	ostr << "\tif (auto " << res << " = " << s << ") {\n";
		// else
		if (r->getRelation()->toBuiltin() == Relation::BuiltinID::po_imm ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::rf ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::rfe ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::rfi ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::mo_imm ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::moe ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::moi ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::tj ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::tc ||
		    r->getRelation()->toBuiltin() == Relation::BuiltinID::frees ||
		    (r->getRelation()->toBuiltin() == Relation::BuiltinID::alloc &&
		     r->getRelation()->isInverse())) {
			ostr << "if (auto " << res << " = " << s << "(g, " << arg << "); " << res
			     << ")";
		} else if (r->getRelation()->toBuiltin() == Relation::BuiltinID::data_imm ||
			   r->getRelation()->toBuiltin() == Relation::BuiltinID::ctrl_imm ||
			   r->getRelation()->toBuiltin() == Relation::BuiltinID::addr_imm) {
			ostr << "for (auto &p : " << s << "(g, " << arg << ")) if (auto *" << res
			     << " = g.getEventLabel(p); true)";
		} else {
			ostr << "for (auto &tmp : " << s << "(g, " << arg << ")) if (auto *" << res
			     << " = &tmp; true)";
		}
		return;
	}

	auto index = getCalcIdx(r->getCalcIndex());
	if (viewCalcs.contains(r->getCalcIndex())) {
		ostr << "FOREACH_MAXIMAL(" << res << ", g, " << arg << "->view(" << index << "))";
		return;
	}
	ostr << "for (auto &" << res << " : " << arg << "->calculated(" << index << "))";
}

void Printer::printTransLabel(const TransLabel *t, const std::string &res, const std::string &arg)
{
	printPredSet(cpp(), arg, t->getPreChecks());
	printRelation(cpp(), res, arg, t);
	printPredSet(cpp(), res, t->getPostChecks());
}

void Printer::printHppHeader()
{
	hpp() << genmcCopyright << "\n" << katerNotice << "\n";

	hpp() << "#ifndef " << guardName << "\n"
	      << "#define " << guardName << "\n"
	      << "\n"
	      << "#include \"config.h\"\n"
	      << "#include \"ExecutionGraph.hpp\"\n"
	      << "#include \"GenMCDriver.hpp\"\n"
	      << "#include \"GraphIterators.hpp\"\n"
	      << "#include \"MaximalIterator.hpp\"\n"
	      << "#include \"VerificationError.hpp\"\n"
	      << "#include \"VSet.hpp\"\n"
	      << "#include <cstdint>\n"
	      << "#include <vector>\n"
	      << "\n"
	      << "class " << className << " : public GenMCDriver {\n"
	      << "\n"
	      << "private:\n"
	      << "\tenum class NodeStatus : unsigned char { unseen, entered, "
		 "left };\n"
	      << "\n"
	      << "\tstruct NodeCountStatus {\n"
	      << "\t\tNodeCountStatus() = default;\n"
	      << "\t\tNodeCountStatus(uint16_t c, NodeStatus s) : count(c), "
		 "status(s) {}\n"
	      << "\t\tuint16_t count = 0;\n"
	      << "\t\tNodeStatus status = NodeStatus::unseen;\n"
	      << "\t};\n"
	      << "\n"
	      << "public:\n"
	      << "\t" << className
	      << "(std::shared_ptr<const Config> conf, std::unique_ptr<llvm::Module> mod,\n"
	      << "\t\tstd::unique_ptr<ModuleInfo> MI, GenMCDriver::Mode mode = "
		 "GenMCDriver::VerificationMode{});\n"
	      << "\n"
	      << "\tstd::vector<VSet<Event>> calculateSaved(const EventLabel *lab);\n"
	      << "\tstd::vector<View> calculateViews(const EventLabel *lab);\n"
	      << "\tvoid updateMMViews(EventLabel *lab) override;\n"
	      << "\tbool isDepTracking() const override;\n"
	      << "\tbool isConsistent(const EventLabel *lab) const override;\n"
	      << "\tVerificationError checkErrors(const EventLabel *lab, const EventLabel *&race) "
		 "const override;\n"
	      << "\tstd::vector<VerificationError> checkWarnings(const EventLabel *lab, const "
		 "VSet<VerificationError> &seenWarnings, std::vector<const EventLabel *> "
		 "&racyLabs) const;\n"
	      << "\tbool isRecoveryValid(const EventLabel *lab) const override;\n"
	      << "\tstd::unique_ptr<VectorClock> calculatePrefixView(const EventLabel *lab) const "
		 "override;\n"
	      << "\tconst View &getHbView(const EventLabel *lab) const override;\n"
	      << "\tstd::vector<Event> getCoherentStores(SAddr addr, Event read) override;\n"
	      << "\tstd::vector<Event> getCoherentRevisits(const WriteLabel "
		 "*sLab, const VectorClock &pporf) override;\n"
	      << "\tllvm::iterator_range<ExecutionGraph::co_iterator>\n"
	      << "\tgetCoherentPlacings(SAddr addr, Event store, bool isRMW) override;\n"
	      << "\n"
	      << "private:\n"
	      << "\tbool isWriteRfBefore(Event a, Event b);\n"
	      << "\tstd::vector<Event> getInitRfsAtLoc(SAddr addr);\n"
	      << "\tbool isHbOptRfBefore(const Event e, const Event write);\n"
	      << "\tExecutionGraph::co_iterator splitLocMOBefore(SAddr addr, Event e);\n"
	      << "\tExecutionGraph::co_iterator splitLocMOAfterHb(SAddr addr, const Event read);\n"
	      << "\tExecutionGraph::co_iterator splitLocMOAfter(SAddr addr, const Event e);\n"
	      << "\tstd::vector<Event> getMOOptRfAfter(const WriteLabel *sLab);\n"
	      << "\tstd::vector<Event> getMOInvOptRfAfter(const WriteLabel *sLab);\n"
	      << "\n";
}

void Printer::printCppHeader()
{
	cpp() << genmcCopyright << "\n" << katerNotice << "\n";

	cpp() << "#include \"" << className << ".hpp\"\n"
	      << "#include \"ModuleInfo.hpp\"\n"
	      << "\n"
	      << className << "::" << className
	      << "(std::shared_ptr<const Config> conf, std::unique_ptr<llvm::Module> mod,\n"
	      << "\t\tstd::unique_ptr<ModuleInfo> MI, GenMCDriver::Mode mode /* = "
		 "GenMCDriver::VerificationMode{} */)\n"
	      << "\t: GenMCDriver(conf, std::move(mod), std::move(MI), mode) {}\n"
	      << "\n";
}

void Printer::printHppFooter()
{
	hpp() << "\tmutable std::vector<VSet<Event>> saved;\n"
	      << "\tmutable std::vector<View> views;\n"
	      << "\n"
	      << "};\n"
	      << "\n"
	      << "#endif /* " << guardName << " */\n";
}

void Printer::printCppFooter() {}

void Printer::outputHpp(const CNFAs &cnfas)
{
	printHppHeader();

	auto i = 0U;
	std::for_each(cnfas.save_begin(), cnfas.save_end(), [&](auto &nfaStatus) {
		printCalculatorHpp(nfaStatus.first, i++, nfaStatus.second);
	});

	i = 0U;
	std::for_each(cnfas.incl_begin(), cnfas.incl_end(), [&](auto &kv) {
		auto &nfaPair = kv.first;
		printInclusionHpp(nfaPair.lhs, nfaPair.rhs, i++,
				  (kv.second == -1 ? std::nullopt : std::make_optional(kv.second)));
	});

	i = 0U;
	for (auto &acyc : cnfas.acycs())
		printAcyclicHpp(std::get<NFA>(acyc), std::get<std::optional<NFA>>(acyc),
				std::get<std::string>(acyc), i++);

	printRecoveryHpp(cnfas.getRecovery());

	printPPoRfHpp(cnfas.getPPoRf().first, cnfas.getPPoRf().second);

	printHppFooter();
}

void Printer::outputCpp(const CNFAs &cnfas)
{
	printCppHeader();

	/* Print calculators + calculateSaved() + calculateViews()*/
	auto i = 0U;
	auto vc = 0U;
	auto sc = 0U;
	std::for_each(cnfas.save_begin(), cnfas.save_end(), [&](auto &nfaStatus) {
		calcToIdxMap.push_back((nfaStatus.second == VarStatus::View) ? vc++ : sc++);
		if (nfaStatus.second == VarStatus::View) {
			viewCalcs.insert(i);
		}
		printCalculatorCpp(nfaStatus.first, i++, nfaStatus.second);
	});
	cpp() << "std::vector<VSet<Event>> " << className
	      << "::calculateSaved(const EventLabel *lab)\n"
	      << "{\n";
	i = 0U;
	std::for_each(cnfas.save_begin(), cnfas.save_end(), [&](auto &nfaStatus) {
		if (nfaStatus.second != VarStatus::View) {
			cpp() << "\tsaved.push_back(calculate" << i << "(lab));\n";
		}
		++i;
	});
	cpp() << "\treturn std::move(saved);\n"
	      << "}\n"
	      << "\n";
	cpp() << "std::vector<View> " << className << "::calculateViews(const EventLabel *lab)\n"
	      << "{\n";
	i = 0U;
	std::for_each(cnfas.save_begin(), cnfas.save_end(), [&](auto &nfaStatus) {
		if (nfaStatus.second == VarStatus::View) {
			cpp() << "\tviews.push_back(calculate" << i << "(lab));\n";
		}
		++i;
	});
	cpp() << "\treturn std::move(views);\n"
	      << "}\n"
	      << "\n";

	cpp() << "void " << className << "::updateMMViews(EventLabel *lab)\n"
	      << "{\n"
	      << "\tlab->setCalculated(calculateSaved(lab));\n"
	      << "\tlab->setViews(calculateViews(lab));\n";
	if (!cnfas.isDepTracking()) { /* we can optimize the calculation */
		cpp() << "\tlab->setPrefixView(calculatePrefixView(lab));\n";
	}
	cpp() << "}\n"
	      << "\n";

	cpp() << "bool " << className << "::isDepTracking() const\n"
	      << "{\n"
	      << "\treturn " << cnfas.isDepTracking() << ";\n"
	      << "}\n"
	      << "\n";

	/* Print inclusion helper */
	i = 0U;
	std::for_each(cnfas.incl_begin(), cnfas.incl_end(), [&](auto &kv) {
		auto &nfaPair = kv.first;
		printInclusionCpp(nfaPair.lhs, nfaPair.rhs, i++,
				  (kv.second == -1 ? std::nullopt : std::make_optional(kv.second)));
	});

	/* Print error inclusions */
	cpp() << "VerificationError " << className
	      << "::checkErrors(const EventLabel *lab, const EventLabel *&race) const\n"
	      << "{";
	i = 0U;
	std::for_each(cnfas.incl_begin(), cnfas.incl_end(), [&](auto &kv) {
		auto &nfaPair = kv.first;
		if (nfaPair.type != Constraint::Type::Error) {
			++i;
			return;
		}
		cpp() << "\n\tif (!checkInclusion" << i << "(lab)) {"
		      << "\n\t\trace = racyLab" << i << ";"
		      << "\n\t\treturn VerificationError::" << nfaPair.s << ";"
		      << "\n\t}\n";
		++i;
	});
	cpp() << "\n\treturn VerificationError::VE_OK;\n"
	      << "}\n"
	      << "\n";

	/* Print warning inclusions */
	cpp() << "std::vector<VerificationError> " << className
	      << "::checkWarnings(const EventLabel *lab, const VSet<VerificationError> "
		 "&seenWarnings, std::vector<const EventLabel *> &racyLabs) const\n"
	      << "{\n"
	      << "\tstd::vector<VerificationError> result;\n";
	i = 0U;
	std::for_each(cnfas.incl_begin(), cnfas.incl_end(), [&](auto &kv) {
		auto &nfaPair = kv.first;
		if (nfaPair.type != Constraint::Type::Warning) {
			++i;
			return;
		}
		cpp() << "\n\tif (seenWarnings.count(VerificationError::" << nfaPair.s
		      << ") == 0 && !checkInclusion" << i << "(lab)) {"
		      << "\n\t\tracyLabs.push_back(racyLab" << i << ");"
		      << "\n\t\tresult.push_back(VerificationError::" << nfaPair.s << ");"
		      << "\n\t}\n";
		++i;
	});
	cpp() << "\n\treturn result;\n"
	      << "}\n"
	      << "\n";

	/* Print acyclicity routines + isConsistent() */
	i = 0U;
	for (auto &acyc : cnfas.acycs())
		printAcyclicCpp(std::get<NFA>(acyc), std::get<std::optional<NFA>>(acyc),
				std::get<std::string>(acyc), i++);
	cpp() << "bool " << className << "::isConsistent(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\n\treturn true";
	i = 0U;
	for (auto &acyc : cnfas.acycs())
		cpp() << "\n\t\t&& isAcyclic" << i << "(lab)";
	cpp() << ";\n"
	      << "}\n"
	      << "\n";

	printRecoveryCpp(cnfas.getRecovery());
	cpp() << "bool " << className << "::isRecoveryValid(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\treturn isRecAcyclic(lab);\n"
	      << "}\n"
	      << "\n";

	/* pporf-before getter */
	printPPoRfCpp(cnfas.getPPoRf().first, cnfas.getPPoRf().second);
	cpp() << "std::unique_ptr<VectorClock> " << className
	      << "::calculatePrefixView(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\treturn std::make_unique<" << (cnfas.getPPoRf().second ? "DepView" : "View")
	      << ">(calcPPoRfBefore(lab));\n"
	      << "}\n"
	      << "\n";

	/* hb-view getter */
	cpp() << "const View &" << className << "::getHbView(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\treturn lab->view(" << getCalcIdx(cnfas.getHbIndex()) << ");\n"
	      << "}\n"
	      << "\n";

	/* coherence utils */
	auto s = std::string(coherenceFuns);
	replaceAll(s, "#CLASS#", className);
	replaceAll(s, "#HB#", std::to_string(getCalcIdx(cnfas.getCohIndex())));
	cpp() << s << "\n";

	printCppFooter();
}

void Printer::output(const CNFAs &cnfas)
{
	outputHpp(cnfas);
	outputCpp(cnfas);
}

template <typename ITER>
auto assignStateIDs(ITER &&begin, ITER &&end) -> std::unordered_map<NFA::State *, unsigned>
{
	std::unordered_map<NFA::State *, unsigned> result;

	auto i = 0U;
	std::for_each(begin, end, [&](auto &s) { result[&*s] = i++; });
	return result;
}

#define GET_ID(nfa_id, state_id) nfa_id << "_" << state_id

void Printer::printAcyclicHpp(const NFA &nfa, const std::optional<NFA> &unlessNFA,
			      const std::string &genmc, unsigned id)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* visitAcyclicXX() for each state */
	for (const auto &sUP : nfa.states()) {
		hpp() << "\tbool visitAcyclic" << GET_ID(id, ids[&*sUP])
		      << "(const EventLabel *lab)"
		      << " const;\n";
	}
	hpp() << "\n";

	/* isAcyclic() for the automaton */
	hpp() << "\tbool isAcyclic" << id << "(const EventLabel *lab)"
	      << " const;\n"
	      << "\n";

	/* status arrays */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeCountStatus> visitedAcyclic"
		      << GET_ID(id, ids[&*s]) << ";\n";
	});
	hpp() << "\n";

	/* accepting counter */
	hpp() << "\tmutable uint16_t visitedAccepting" << id << " = 0;\n";

	/* If there is no code for "unless", make shouldVisitAcyclicX return true */
	if (!unlessNFA.has_value()) {
		hpp() << "\tbool shouldVisitAcyclic" << id << "(void) const { return true; };\n";
		return;
	}

	auto uids = assignStateIDs(unlessNFA->states_begin(), unlessNFA->states_end());

	/* shouldVisitAcyclicXX() for each state */
	for (const auto &sUP : unlessNFA->states())
		hpp() << "\tbool shouldVisitAcyclic" << GET_ID(id, uids[&*sUP])
		      << "(const EventLabel *lab) const;\n";
	hpp() << "\n";

	/* shouldVisitAcyclicX() for the automaton */
	hpp() << "\tbool shouldVisitAcyclic" << id << "(void) const;\n"
	      << "\n";

	/* more status arrays */
	for (const auto &sUP : unlessNFA->states())
		hpp() << "\tmutable std::vector<NodeStatus> shouldVisitedAcyclic"
		      << GET_ID(id, uids[&*sUP]) << ";\n";
	hpp() << "\n";
}

void Printer::printAcyclicCpp(const NFA &nfa, const std::optional<NFA> &unlessNFA,
			      const std::string &genmc, unsigned id)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* Print a "visitAcyclicXX" for each state */
	for (const auto &sUP : nfa.states()) {
		cpp() << "bool " << className << "::visitAcyclic" << GET_ID(id, ids[&*sUP])
		      << "(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n";

		if (sUP->isStarting()) {
			cpp() << "\t++visitedAccepting" << id << ";\n";
		}
		cpp() << "\tvisitedAcyclic" << GET_ID(id, ids[&*sUP])
		      << "[lab->getStamp().get()] = "
			 "{ visitedAccepting"
		      << id << ", NodeStatus::entered };\n";
		std::for_each(sUP->in_begin(), sUP->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto &node = visitedAcyclic" << GET_ID(id, ids[t.dest])
			      << "[pLab->getStamp().get()];\n"
			      << "\t\tif (node.status == NodeStatus::unseen && !visitAcyclic"
			      << GET_ID(id, ids[t.dest]) << "(pLab))\n"
			      << "\t\t\treturn false;\n"
			      << "\t\telse if (node.status == NodeStatus::entered && "
				 "visitedAccepting"
			      << id << " > node.count)\n"
			      << "\t\t\treturn false;\n"
			      << "\t}\n";
		});
		if (sUP->isStarting()) {
			cpp() << "\t--visitedAccepting" << id << ";\n";
		}
		cpp() << "\tvisitedAcyclic" << GET_ID(id, ids[&*sUP])
		      << "[lab->getStamp().get()] = "
			 "{ visitedAccepting"
		      << id << ", NodeStatus::left };\n"
		      << "\treturn true;\n"
		      << "}\n"
		      << "\n";
	}

	/* Print a "isAcyclicX" for the automaton */
	cpp() << "bool " << className << "::isAcyclic" << id << "(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\tauto &g = getGraph();\n"
	      << "\n"
	      << genmc << "\n"
	      << "\tif (!shouldVisitAcyclic" << id << "())\n"
	      << "\t\treturn true;\n"
	      << "\n"
	      << "\tvisitedAccepting" << id << " = 0;\n";
	for (auto &sUP : nfa.states()) {
		cpp() << "\tvisitedAcyclic" << GET_ID(id, ids[&*sUP]) << ".clear();\n"
		      << "\tvisitedAcyclic" << GET_ID(id, ids[&*sUP])
		      << ".resize(g.getMaxStamp().get() + 1);\n";
	}
	cpp() << "\treturn true";
	for (auto &sUP : nfa.states()) {
		if (std::ranges::all_of(sUP->outs(), [&](auto &t) {
			    auto rOpt = t.label.getRelation();
			    return rOpt.has_value() && !rOpt->isInverse() &&
				   (getModule().getTheory().isIncludedIn(
					    *rOpt,
					    Relation::createBuiltin(Relation::BuiltinID::po_imm)) ||
				    getModule().getTheory().isIncludedIn(
					    *rOpt,
					    Relation::createBuiltin(Relation::BuiltinID::rf)) ||
				    !rOpt->isBuiltin());
		    })) {
			continue;
		}
		cpp() << "\n\t\t&& visitAcyclic" << GET_ID(id, ids[&*sUP]) << "(lab)";
	}
	cpp() << ";\n"
	      << "}\n"
	      << "\n";

	/* If there is no "unless" clause don't bother; shouldVisitAcyclic has been defined in the
	 * header file */
	if (!unlessNFA.has_value())
		return;

	auto uids = assignStateIDs(unlessNFA->states_begin(), unlessNFA->states_end());

	/* Print a "shouldVisitAcyclicXX" for each state */
	for (const auto &sUP : unlessNFA->states()) {
		cpp() << "bool " << className << "::shouldVisitAcyclic" << GET_ID(id, uids[&*sUP])
		      << "(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n"
		      << "\tshouldVisitedAcyclic" << GET_ID(id, uids[&*sUP])
		      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
		if (sUP->isStarting())
			cpp() << "\treturn false;\n";
		for (const auto &t : sUP->ins()) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto &status = shouldVisitedAcyclic"
			      << GET_ID(id, uids[t.dest]) << "[pLab->getStamp().get()];\n"
			      << "\t\tif (status == NodeStatus::unseen && !shouldVisitAcyclic"
			      << GET_ID(id, uids[t.dest]) << "(pLab))\n"
			      << "\t\t\treturn false;\n"
			      << "\t}\n";
		}
		cpp() << "\tshouldVisitedAcyclic" << GET_ID(id, uids[&*sUP])
		      << "[lab->getStamp().get()] = NodeStatus::left;\n"
		      << "\treturn true;\n"
		      << "}\n"
		      << "\n";
	}

	/* Print a proper "shouldVisitAcyclicX" for the automaton */
	cpp() << "bool " << className << "::shouldVisitAcyclic" << id << "(void) const\n"
	      << "{\n"
	      << "\tauto &g = getGraph();\n"
	      << "\n";
	for (auto &sUP : unlessNFA->states()) {
		cpp() << "\tshouldVisitedAcyclic" << GET_ID(id, uids[&*sUP]) << ".clear();\n"
		      << "\tshouldVisitedAcyclic" << GET_ID(id, uids[&*sUP])
		      << ".resize(g.getMaxStamp().get() + 1);\n";
	}
	cpp() << "\treturn false";
	for (auto &sUP : unlessNFA->accepting())
		cpp() << "\n\t\t|| std::any_of(label_begin(g), label_end(g), [&](auto &lab){ "
			 "return !shouldVisitAcyclic"
		      << GET_ID(id, uids[&*sUP]) << "(&lab); })";
	cpp() << ";\n"
	      << "}\n"
	      << "\n";
}

void Printer::printRecoveryHpp(const NFA &nfa)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* visitRecoveryX() for each state */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tbool visitRecovery" << ids[&*s] << "(const EventLabel *lab)"
		      << ";\n";
	});
	hpp() << "\n";

	/* isRecAcyclic() for the automaton */
	hpp() << "\tbool isRecAcyclic(const EventLabel *lab)"
	      << " const;\n"
	      << "\n";

	/* status arrays */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeCountStatus> visitedRecovery" << ids[&*s]
		      << ";\n";
	});
	hpp() << "\n";

	/* accepting counter */
	hpp() << "\tmutable uint16_t visitedRecAccepting = 0;\n";
}

void Printer::printRecoveryCpp(const NFA &nfa)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* Print a "visitRecoveryXX" for each state */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "bool " << className << "::visitRecovery" << ids[&*s]
		      << "(const EventLabel *lab)\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n";

		if (s->isStarting()) {
			cpp() << "\t++visitedRecAccepting;\n";
		}
		cpp() << "\tvisitedRecovery" << ids[&*s]
		      << "[lab->getStamp().get()] = "
			 "{ visitedRecAccepting, NodeStatus::entered };\n";
		std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto &node = visitedRecovery" << ids[t.dest]
			      << "[pLab->getStamp().get()];\n"
			      << "\t\tif (node.status == NodeStatus::unseen && !visitRecovery"
			      << ids[t.dest] << "(p))\n"
			      << "\t\t\treturn false;\n"
			      << "\t\telse if (node.status == NodeStatus::entered && "
				 "visitedRecAccepting > node.count)\n"
			      << "\t\t\treturn false;\n"
			      << "\t}\n";
		});
		if (s->isStarting()) {
			cpp() << "\t--visitedRecAccepting;\n";
		}
		cpp() << "\tvisitedRecovery" << ids[&*s]
		      << "[lab->getStamp().get()] = "
			 "{ visitedRecAccepting, NodeStatus::left };\n"
		      << "\treturn true;\n"
		      << "}\n"
		      << "\n";
	});

	/* Print a "isRecAcyclicX" for the automaton */
	cpp() << "bool " << className << "::isRecAcyclic(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\tvisitedRecAccepting = 0;\n";
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "\tvisitedRecovery" << ids[&*s] << ".clear();\n"
		      << "\tvisitedRecovery" << ids[&*s] << ".resize(g.getMaxStamp().get() + 1);\n";
	});
	cpp() << "\treturn true";
	std::for_each(nfa.states_begin(), nfa.states_end(),
		      [&](auto &s) { cpp() << "\n\t\t&& visitRecovery" << ids[&*s] << "(lab)"; });
	cpp() << ";\n"
	      << "}\n"
	      << "\n";
}

void Printer::printCalculatorHpp(const NFA &nfa, unsigned id, VarStatus status)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* visitCalcXX for each state */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tvoid visitCalc" << GET_ID(id, ids[&*s]) << "(const EventLabel *lab, "
		      << ((status == VarStatus::View) ? "View &" : "VSet<Event> &")
		      << "calcRes);\n";
	});
	hpp() << "\n";

	/* calculateX for the automaton */
	hpp() << "\t" << ((status == VarStatus::View) ? "View" : "VSet<Event>") << " calculate"
	      << id << "(const EventLabel *lab);\n";
	hpp() << "\n";

	/* status arrays */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeStatus> visitedCalc" << GET_ID(id, ids[&*s])
		      << ";\n";
	});
	hpp() << "\n";
}

void Printer::printCalculatorCpp(const NFA &nfa, unsigned id, VarStatus status)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "void " << className << "::visitCalc" << GET_ID(id, ids[&*s])
		      << "(const EventLabel *lab, "
		      << ((status == VarStatus::View) ? "View &" : "VSet<Event> &") << "calcRes)\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n"
		      << "\tvisitedCalc" << GET_ID(id, ids[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
		if (s->isStarting()) {
			if (status != VarStatus::View) {
				cpp() << "\tcalcRes.insert(lab->getPos());\n";
			}
			if (status == VarStatus::Reduce) {
				cpp() << "\tfor (const auto &p : lab->calculated(" << getCalcIdx(id)
				      << ")) {\n"
				      << "\t\tcalcRes.erase(p);\n";
				std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto &a) {
					cpp() << "\t\tvisitedCalc" << GET_ID(id, ids[a])
					      << "[g.getEventLabel(p)->getStamp().get()] = "
						 "NodeStatus::left;\n";
				});
				cpp() << "\t}\n";
			} else if (status == VarStatus::View) {
				cpp() << "\tcalcRes.update(lab->view(" << getCalcIdx(id) << "));\n"
				      << "\tcalcRes.updateIdx(lab->getPos());\n";
			}
		}
		std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto status = visitedCalc" << GET_ID(id, ids[t.dest])
			      << "[pLab->getStamp().get()];\n"
			      << "\t\tif (status == NodeStatus::unseen)\n"
			      << "\t\t\tvisitCalc" << GET_ID(id, ids[t.dest])
			      << "(pLab, calcRes);\n"
			      << "\t}\n";
		});
		cpp() << "\tvisitedCalc" << GET_ID(id, ids[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::left;\n"
		      << "}\n"
		      << "\n";
	});

	cpp() << ((status == VarStatus::View) ? "View " : "VSet<Event> ") << className
	      << "::calculate" << id << "(const EventLabel *lab)\n"
	      << "{\n"
	      << "\tauto &g = getGraph();\n"
	      << "\t" << ((status == VarStatus::View) ? "View" : "VSet<Event>") << " calcRes;\n"
	      << "\n";
	if (status == VarStatus::View) {
		cpp() << "\tcalcRes.updateIdx(lab->getPos().prev());\n";
	}
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "\tvisitedCalc" << GET_ID(id, ids[&*s]) << ".clear();\n"
		      << "\tvisitedCalc" << GET_ID(id, ids[&*s])
		      << ".resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);\n";
	});
	cpp() << "\n";
	std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto &a) {
		cpp() << "\tvisitCalc" << GET_ID(id, ids[a]) << "(lab, calcRes);\n";
	});
	cpp() << "\treturn calcRes;\n"
	      << "}\n";
}

void Printer::printPPoRfHpp(const NFA &nfa, bool deps)
{
	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	/* visitPPoRfXX for each state */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tvoid visitPPoRf" << ids[&*s] << "(const EventLabel *lab, "
		      << (deps ? "DepView" : "View") << " &pporf) const;\n";
	});
	hpp() << "\n";

	/* calcPPoRfBefore for the automaton */
	hpp() << "\t" << (deps ? "DepView" : "View")
	      << " calcPPoRfBefore(const EventLabel *lab) const;\n";
	hpp() << "\n";

	/* status arrays */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeStatus> visitedPPoRf" << ids[&*s] << ";\n";
	});
	hpp() << "\n";
}

void Printer::printPPoRfCpp(const NFA &nfa, bool deps)
{
	/* Optimization for non-dep-tracking models */
	if (!deps) {
		cpp() << (deps ? "DepView " : "View ") << className
		      << "::calcPPoRfBefore(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\tView pporf;\n"
		      << "\tpporf.updateIdx(lab->getPos());\n"
		      << "\n"
		      << "\tauto *pLab = g.getPreviousLabel(lab);\n"
		      << "\tif (!pLab)\n"
		      << "\t\treturn pporf;\n"
		      << "\tpporf.update(pLab->getPrefixView());\n"
		      << "\tif (auto *rLab = llvm::dyn_cast<ReadLabel>(pLab))\n"
		      << "\t\tpporf.update(rLab->getRf()->getPrefixView());\n"
		      << "\tif (auto *tsLab = llvm::dyn_cast<ThreadStartLabel>(pLab))\n"
		      << "\t\tpporf.update(g.getEventLabel(tsLab->getParentCreate())->"
			 "getPrefixView());\n"
		      << "\tif (auto *tjLab = llvm::dyn_cast<ThreadJoinLabel>(pLab))\n"
		      << "\t\tpporf.update(g.getLastThreadLabel(tjLab->getChildId())->"
			 "getPrefixView());\n"
		      << "\treturn pporf;\n"
		      << "}\n";
		return;
	}

	auto ids = assignStateIDs(nfa.states_begin(), nfa.states_end());

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "void " << className << "::visitPPoRf" << ids[&*s]
		      << "(const EventLabel *lab, " << (deps ? "DepView" : "View")
		      << " &pporf) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n"
		      << "\tvisitedPPoRf" << ids[&*s]
		      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
		if (s->isStarting()) {
			cpp() << "\tpporf.updateIdx(lab->getPos());\n";
		}
		std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto status = visitedPPoRf" << ids[t.dest]
			      << "[pLab->getStamp().get()];\n"
			      << "\t\tif (status == NodeStatus::unseen)\n"
			      << "\t\t\tvisitPPoRf" << ids[t.dest] << "(pLab, pporf);\n"
			      << "\t}\n";
		});
		cpp() << "\tvisitedPPoRf" << ids[&*s]
		      << "[lab->getStamp().get()] = NodeStatus::left;\n"
		      << "}\n"
		      << "\n";
	});

	cpp() << (deps ? "DepView " : "View ") << className
	      << "::calcPPoRfBefore(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\tauto &g = getGraph();\n"
	      << "\t" << (deps ? "DepView" : "View") << " pporf;\n"
	      << "\tpporf.updateIdx(lab->getPos());\n";
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		cpp() << "\tvisitedPPoRf" << ids[&*s] << ".clear();\n"
		      << "\tvisitedPPoRf" << ids[&*s]
		      << ".resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);\n";
	});
	cpp() << "\n";
	std::for_each(nfa.accept_begin(), nfa.accept_end(),
		      [&](auto &a) { cpp() << "\tvisitPPoRf" << ids[a] << "(lab, pporf);\n"; });
	cpp() << "\treturn pporf;\n"
	      << "}\n";
}

void Printer::printInclusionHpp(const NFA &lhs, const NFA &rhs, unsigned id,
				std::optional<unsigned> rhsViewIdx)
{
	auto idsLHS = assignStateIDs(lhs.states_begin(), lhs.states_end());
	auto idsRHS = assignStateIDs(rhs.states_begin(), rhs.states_end());

	if (rhsViewIdx.has_value()) {
		/* bool visitInclusionXX for LHS only */
		std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
			hpp() << "\tbool visitInclusionLHS" << GET_ID(id, idsLHS[&*s])
			      << "(const EventLabel *lab, const View &v)"
			      << " const;\n";
		});
	} else {
		/* visitInclusionXX for each state */
		std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
			hpp() << "\tvoid visitInclusionLHS" << GET_ID(id, idsLHS[&*s])
			      << "(const EventLabel *lab)"
			      << " const;\n";
		});
		std::for_each(rhs.states_begin(), rhs.states_end(), [&](auto &s) {
			hpp() << "\tvoid visitInclusionRHS" << GET_ID(id, idsRHS[&*s])
			      << "(const EventLabel *lab)"
			      << " const;\n";
		});
	}
	hpp() << "\n";

	/* checkInclusionX for the automaton */
	hpp() << "\tbool checkInclusion" << id << "(const EventLabel *lab)"
	      << " const;\n"
	      << "\n";

	/* status arrays */
	std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeStatus> visitedInclusionLHS"
		      << GET_ID(id, idsLHS[&*s]) << ";\n";
	});
	std::for_each(rhs.states_begin(), rhs.states_end(), [&](auto &s) {
		hpp() << "\tmutable std::vector<NodeStatus> visitedInclusionRHS"
		      << GET_ID(id, idsRHS[&*s]) << ";\n";
	});
	hpp() << "\n";

	/* caches for inclusion checks */
	hpp() << "\tmutable std::vector<bool> lhsAccept" << id << ";\n"
	      << "\tmutable std::vector<bool> rhsAccept" << id << ";\n"
	      << "\n";

	/* cache for racy event found */
	hpp() << "\tmutable const EventLabel *racyLab" << id << " = nullptr;\n"
	      << "\n";
}

void Printer::printInclusionCpp(const NFA &lhs, const NFA &rhs, unsigned id,
				std::optional<unsigned> rhsViewIdx)
{
	auto idsLHS = assignStateIDs(lhs.states_begin(), lhs.states_end());

	if (rhsViewIdx.has_value()) {
		std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
			cpp() << "bool " << className << "::visitInclusionLHS"
			      << GET_ID(id, idsLHS[&*s])
			      << "(const EventLabel *lab, const View &v) const\n"
			      << "{\n"
			      << "\tauto &g = getGraph();\n"
			      << "\n"
			      << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
			      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
			if (s->isStarting()) {
				cpp() << "\tif (!v.contains(lab->getPos())) {\n"
				      << "\t\tracyLab" << id << " = lab;\n"
				      << "\t\treturn false;\n"
				      << "\t}\n";
			}
			std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
				cpp() << "\t";
				printTransLabel(&t.label, "pLab", "lab");
				cpp() << " {\n"
				      << "\t\tauto status = visitedInclusionLHS"
				      << GET_ID(id, idsLHS[t.dest]) << "[pLab->getStamp().get()];\n"
				      << "\t\tif (status == NodeStatus::unseen && "
					 "!visitInclusionLHS"
				      << GET_ID(id, idsLHS[t.dest]) << "(pLab, v))\n"
				      << "\t\t\treturn false;\n"
				      << "\t}\n";
			});
			cpp() << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
			      << "[lab->getStamp().get()] = NodeStatus::left;\n"
			      << "\treturn true;\n"
			      << "}\n"
			      << "\n";
		});

		cpp() << "bool " << className << "::checkInclusion" << id
		      << "(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\tauto &v = lab->view(" << getCalcIdx(*rhsViewIdx) << ");\n"
		      << "\n";
		std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
			cpp() << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s]) << ".clear();\n"
			      << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
			      << ".resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);\n";
		});
		cpp() << "\treturn true";
		std::for_each(lhs.accept_begin(), lhs.accept_end(), [&](auto &s) {
			cpp() << "\n\t\t&& visitInclusionLHS" << GET_ID(id, idsLHS[s])
			      << "(lab, v)";
		});
		cpp() << ";\n"
		      << "}\n"
		      << "\n";
		return;
	}

	std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
		cpp() << "void " << className << "::visitInclusionLHS" << GET_ID(id, idsLHS[&*s])
		      << "(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n"
		      << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
		if (s->isStarting()) {
			cpp() << "\tlhsAccept" << id << "[lab->getStamp().get()] = true;\n";
		}
		std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto status = visitedInclusionLHS"
			      << GET_ID(id, idsLHS[t.dest]) << "[pLab->getStamp().get()];\n"
			      << "\t\tif (status == NodeStatus::unseen)\n"
			      << "\t\t\tvisitInclusionLHS" << GET_ID(id, idsLHS[t.dest])
			      << "(pLab);\n"
			      << "\t}\n";
		});
		cpp() << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::left;\n"
		      << "}\n"
		      << "\n";
	});

	auto idsRHS = assignStateIDs(rhs.states_begin(), rhs.states_end());
	std::for_each(rhs.states_begin(), rhs.states_end(), [&](auto &s) {
		cpp() << "void " << className << "::visitInclusionRHS" << GET_ID(id, idsRHS[&*s])
		      << "(const EventLabel *lab) const\n"
		      << "{\n"
		      << "\tauto &g = getGraph();\n"
		      << "\n"
		      << "\tvisitedInclusionRHS" << GET_ID(id, idsRHS[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::entered;\n";
		if (s->isStarting()) {
			cpp() << "\trhsAccept" << id << "[lab->getStamp().get()] = true;\n";
		}
		std::for_each(s->in_begin(), s->in_end(), [&](auto &t) {
			cpp() << "\t";
			printTransLabel(&t.label, "pLab", "lab");
			cpp() << " {\n"
			      << "\t\tauto status = visitedInclusionRHS"
			      << GET_ID(id, idsRHS[t.dest]) << "[pLab->getStamp().get()];\n"
			      << "\t\tif (status == NodeStatus::unseen)\n"
			      << "\t\t\tvisitInclusionRHS" << GET_ID(id, idsRHS[t.dest])
			      << "(pLab);\n"
			      << "\t}\n";
		});
		cpp() << "\tvisitedInclusionRHS" << GET_ID(id, idsRHS[&*s])
		      << "[lab->getStamp().get()] = NodeStatus::left;\n"
		      << "}\n"
		      << "\n";
	});

	cpp() << "bool " << className << "::checkInclusion" << id
	      << "(const EventLabel *lab) const\n"
	      << "{\n"
	      << "\tauto &g = getGraph();\n"
	      << "\n";
	std::for_each(lhs.states_begin(), lhs.states_end(), [&](auto &s) {
		cpp() << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s]) << ".clear();\n"
		      << "\tvisitedInclusionLHS" << GET_ID(id, idsLHS[&*s])
		      << ".resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);\n";
	});
	std::for_each(rhs.states_begin(), rhs.states_end(), [&](auto &s) {
		cpp() << "\tvisitedInclusionRHS" << GET_ID(id, idsRHS[&*s]) << ".clear();\n"
		      << "\tvisitedInclusionRHS" << GET_ID(id, idsRHS[&*s])
		      << ".resize(g.getMaxStamp().get() + 1, NodeStatus::unseen);\n";
	});
	cpp() << "\tlhsAccept" << id << ".clear();\n"
	      << "\tlhsAccept" << id << ".resize(g.getMaxStamp().get() + 1, false);\n"
	      << "\trhsAccept" << id << ".clear();\n"
	      << "\trhsAccept" << id << ".resize(g.getMaxStamp().get() + 1, false);\n"
	      << "\n";

	std::for_each(lhs.accept_begin(), lhs.accept_end(), [&](auto &s) {
		cpp() << "\tvisitInclusionLHS" << GET_ID(id, idsLHS[s]) << "(lab);\n";
	});
	std::for_each(rhs.accept_begin(), rhs.accept_end(), [&](auto &s) {
		cpp() << "\tvisitInclusionRHS" << GET_ID(id, idsRHS[s]) << "(lab);\n";
	});

	cpp() << "\tfor (auto i = 0u; i < lhsAccept" << id << ".size(); i++) {\n"
	      << "\t\tif (lhsAccept" << id << "[i] && !rhsAccept" << id << "[i]) {\n"
	      << "\t\t\tracyLab" << id
	      << " = &*std::find_if(label_begin(g), label_end(g), [&](auto &lab){ return "
		 "lab.getStamp() == i; });\n"
	      << "\t\t\treturn false;\n"
	      << "\t\t}\n"
	      << "\t}\n"
	      << "\treturn true;\n"
	      << "}\n"
	      << "\n";
}
