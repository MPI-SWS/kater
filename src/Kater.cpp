#include "Kater.hpp"
#include "Config.hpp"
#include "Printer.hpp"
#include "Saturation.hpp"
#include "Utils.hpp"
#include "Visitor.hpp"
#include <numeric>
#include <utility>

void Kater::expandSavedVars(URE &r)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		expandSavedVars(r->getKid(i));
	}
	if (auto *re = dynamic_cast<CharRE *>(&*r)) {
		if (re->getLabel().isBuiltin() || !module->isSavedID(re)) {
			return;
		}
		r = module->getSavedID(re);
		expandSavedVars(r);
	}
}

void Kater::expandRfs(URE &r)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		expandRfs(r->getKid(i));
	}

	auto rf = module->getRegisteredID("rf");
	const auto *re = dynamic_cast<const CharRE *>(&*r);
	if ((re == nullptr) || re->getLabel().getRelation() != dynamic_cast<const CharRE *>(&*rf)->getLabel().getRelation()) {
		return;
	}

	auto rfe = module->getRegisteredID("rfe");
	auto rfi = module->getRegisteredID("rfi");

	auto re1 = re->clone();
	auto re2 = re->clone();
	auto l1 = TransLabel(dynamic_cast<CharRE *>(&*rfe)->getLabel().getRelation(),
			     re->getLabel().getPreChecks(),
			     re->getLabel().getPostChecks());
	auto l2 = TransLabel(dynamic_cast<CharRE *>(&*rfi)->getLabel().getRelation(),
			     re->getLabel().getPreChecks(),
			     re->getLabel().getPostChecks());
	dynamic_cast<CharRE *>(&*re1)->setLabel(l1);
	dynamic_cast<CharRE *>(&*re2)->setLabel(l2);

	r = AltRE::createOpt(std::move(re1), std::move(re2));
}

void Kater::printCounterexample(const Counterexample &cex) const
{
	if (cex.empty()) {
		return;
	}

	std::cerr << "Counterexample: ";
	auto i = 0U;
	std::for_each(cex.begin(), cex.end(), [&](auto &lab){
		std::cerr << ((!lab.isRelation() || lab.isBuiltin()) ? lab.toString() :
			      getModule().getRelationName(*lab.getRelation())) << " ";
		if (cex.getType() == Counterexample::Type::TUT && i++ == cex.getMismatch()) {
			std::cerr << "===> ";
		}
	});
	if (cex.getType() == Counterexample::Type::ANA) {
		std::cerr << "(A/NA)";
	}
	std::cerr << "\n";
}

void Kater::registerDefaultAssumptions()
{
	auto rf = module->getRegisteredID("rf");
	auto fr = module->getRegisteredID("fr-imm");
	auto mo = module->getRegisteredID("mo-imm");
	auto seq = SeqRE::createOpt(std::move(rf), std::move(fr));
	auto cons = SubsetConstraint::createOpt(std::move(seq), std::move(mo));
	module->registerAssume(std::move(cons));
}

void ignoreInitAndFinalPreds(NFA &nfa)
{
	std::for_each(nfa.start_begin(), nfa.start_end(), [&](auto &pi){
		if (std::any_of(pi->out_begin(), pi->out_end(),
				[&](auto &t){ return t.label.isPredicate(); })) {
			/* assume normal form */
			assert(std::all_of(pi->out_begin(), pi->out_end(),
					   [&](auto &t){ return t.label.isPredicate(); }));
			nfa.clearAllStarting();
			std::for_each(pi->out_begin(), pi->out_end(), [&](auto &t){
					nfa.makeStarting(t.dest);
			});
		}
	});
	std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto &pf){
		if (std::any_of(pf->in_begin(), pf->in_end(),
				[&](auto &t){ return t.label.isPredicate(); })) {
			assert(std::all_of(pf->in_begin(), pf->in_end(),
					   [&](auto &t){ return t.label.isPredicate(); }));
			nfa.clearAllAccepting();
			std::for_each(pf->in_begin(), pf->in_end(), [&](auto &t){
					nfa.makeAccepting(t.dest);
				});
		}
	});
	nfa.removeDeadStates();
	assert(std::all_of(nfa.start_begin(), nfa.start_end(), [&](auto *s){
		return std::all_of(s->out_begin(), s->out_end(), [&](auto &t){ return !t.label.isPredicate(); });
	}));
	assert(std::all_of(nfa.accept_begin(), nfa.accept_end(), [&](auto *s){
		return std::all_of(s->in_begin(), s->in_end(), [&](auto &t){ return !t.label.isPredicate(); });
	}));
}

struct Path {
	Path(NFA::State *s, NFA::State *e) : start(s), end(e) {}

	auto operator==(const Path &other) const -> bool {
		return start == other.start && end == other.end;
	}
	auto operator<(const Path &other) const -> bool {
		return start < other.start || (start == other.start && end < other.end);
	}

	NFA::State *start;
	NFA::State *end;
};

template<typename T>
inline void hash_combine(std::size_t& seed, std::size_t v)
{
	std::hash<T> hasher;
	seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

auto hasMatchingPathDFS(const NFA &nfa1, const NFA &nfa2) -> bool
{
	struct SPair {
		NFA::State *s1;
		NFA::State *s2;

		auto operator==(const SPair &other) const -> bool {
			return s1 == other.s1 && s2 == other.s2;
		}
	};
	// XXX: FIXME
	struct SPairHasher {
		auto operator()(SPair p) const -> std::size_t {
			std::size_t hash = 0;
			hash_combine<unsigned>(hash, p.s1->getId());
			hash_combine<unsigned>(hash, p.s2->getId());
			return hash;
		}
	};

	struct SimState {
		NFA::State *s1;
		NFA::State *s2;
	};

	std::unordered_set<SPair, SPairHasher> visited;
	std::vector<SimState> workList;

	assert(nfa2.getNumStarting() == 1);
	std::for_each(nfa1.start_begin(), nfa1.start_end(), [&](auto *s1){
		std::for_each(nfa2.start_begin(), nfa2.start_end(), [&](auto *s2){
			visited.insert({s1, s2});
			workList.push_back({s1, s2});
		});
	});

	while (!workList.empty()) {
		auto [s1, s2] = workList.back();
		workList.pop_back();

		if (NFA::isAccepting(s1) && NFA::isAccepting(s2)) {
			return true;
		}

		for (auto it = s1->out_begin(); it != s1->out_end(); ++it) {
			for (auto oit = s2->out_begin(); oit != s2->out_end(); ++oit) {
				if (!oit->label.matches(it->label)) {
					continue;
				}
				if (visited.count({it->dest, oit->dest}) != 0u) {
					continue;
				}

				auto lab = oit->label.getPreChecks();
				lab.minus(it->label.getPreChecks());

				visited.insert({it->dest, oit->dest});
				workList.push_back({it->dest, oit->dest});
			}
		}
	}
	return false;
}

void normalize(NFA &nfa, Constraint::ValidFunT vfun);

auto
findAllMatchingPaths(const NFA &pattern, const NFA &nfa) -> std::vector<std::vector<Path>>
{
	std::vector<std::vector<Path>> result;
	std::unordered_map<NFA::State *, NFA::State *> m;
	std::unordered_map<NFA::State *, NFA::State *> rm;

	auto nfac = nfa.copy(&m);
	std::for_each(m.begin(), m.end(), [&](auto &kv){
		rm[kv.second] = kv.first;
	});

	auto patc = pattern.copy();

	std::for_each(nfac.states_begin(), nfac.states_end(), [&](auto &is){
		nfac.clearAllStarting();
		nfac.makeStarting(&*is);

		std::vector<Path> ps;
		std::for_each(nfac.states_begin(), nfac.states_end(), [&](auto &fs){
			nfac.clearAllAccepting();
			nfac.makeAccepting(&*fs);

			if (hasMatchingPathDFS(patc, nfac)) {
				ps.push_back({rm[&*is], rm[&*fs]});
			}
		});
		result.push_back(ps);
	});
	return result;
}

void findPathsFrom(const NFA &pattern, NFA::State *p,
		   const NFA &nfa, NFA::State *s,
		   Path current, std::vector<Path> &collected)
{
	if (NFA::isAccepting(p)) {
		current.end = s;
		collected.push_back(std::move(current));
		return;
	}

	for (auto it = p->out_begin(); it != p->out_end(); ++it ) {
		const auto &tp = *it;
		/* skip self loops */
		if (std::any_of(tp.dest->out_begin(), tp.dest->out_end(), [&](auto &t){
					return tp.dest->hasIncoming(tp.flipTo(t.dest)); })) {
			continue;
		}
		for (auto oit = s->out_begin(); oit != s->out_end(); ++oit) {
			const auto &ts = *oit;
			/* skip self loops */
			// if (ts.dest == s)
			// 	continue;
			if (!ts.label.matches(tp.label)) {
				continue;
			}
			/* Ensure self loops @ dest match */
			// if (std::any_of(tp.dest->out_begin(), tp.dest->out_end(), [&](auto &tpp){
			// 			return tpp.dest == tp.dest &&
			// 				!nfa.hasTransition(ts.dest,
			// 						   NFA::Transition(tpp.label, ts.dest)) &&
			// 				!nfa.hasTransition(s,
			// 						   NFA::Transition(tpp.label, s));

			// 		}))
			// 	continue;

			findPathsFrom(pattern, tp.dest, nfa, ts.dest, current, collected);
		}
	}
}

auto
findAllMatchingPathsOpt(const NFA &pattern, const NFA &nfa) -> std::vector<std::vector<Path>>

{
	std::vector<std::vector<Path>> result;

	auto patc = pattern.copy();
	ignoreInitAndFinalPreds(patc);

	// assert(patc.getNumStarting() == 1);
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s){
		std::vector<Path> ps;
		std::for_each(patc.start_begin(), patc.start_end(), [&](auto *i){
			findPathsFrom(patc, i, nfa, &*s, Path(&*s, nullptr), ps);
		});
		std::sort(ps.begin(), ps.end());
		ps.erase(std::unique(ps.begin(), ps.end()), ps.end());
		if (!ps.empty()) {
			result.push_back(std::move(ps));
		}
	});
	return result;
}

void expandAssumption(NFA &nfa, const Constraint *assm, const NFA &other)
{
	assert(assm);
	if (const auto *cc = dynamic_cast<const ConjunctiveConstraint *>(assm)) {
		expandAssumption(nfa, cc->getConstraint1(), other);
		expandAssumption(nfa, cc->getConstraint2(), other);
		return;
	}
	if (const auto *tc = dynamic_cast<const TotalityConstraint *>(assm)) {
		saturateTotal(nfa, *dynamic_cast<const CharRE *>(tc->getRelation())->getLabel().getRelation());
		return;
	}
	if (const auto *tc = dynamic_cast<const TransitivityConstraint *>(assm)) {
		saturateTransitive(nfa, *dynamic_cast<const CharRE *>(tc->getRelation())->getLabel().getRelation());
		return;
	}

	const auto *ec = dynamic_cast<const SubsetConstraint *>(assm);
	if (ec == nullptr) {
		std::cout << "Ignoring unsupported local assumption " << *assm << "\n";
		return;
	}

	const auto *lRE = &*ec->getKid(0);
	const auto *rRE = &*ec->getKid(1);
	auto lNFA = lRE->toNFA();
	normalize(lNFA, [](auto & /*t*/){ return true; });

	/* Handle `A <= 0` assumption */
	if (ec->getKid(1)->isFalse()) {
		saturateEmpty(nfa, std::move(lNFA), other);
		return;
	}

	/* Handle `A <= id` assumption */
	if (*rRE == *RegExp::createId()) {
		saturateID(nfa, std::move(lNFA));
		return;
	}

	auto rNFA = rRE->toNFA();
	normalize(rNFA, [](auto & /*t*/){ return true; });

	std::vector<std::vector<Path>> paths;
	// auto *seqRE = dynamic_cast<SeqRE *>(lRE);
	// if (seqRE &&
	//     std::all_of(seqRE->kid_begin(), seqRE->kid_end(), [&](auto &k){
	// 		    return dynamic_cast<CharRE *>(k.get()) ||
	// 			    (dynamic_cast<PlusRE *>(k.get()) &&
	// 			     dynamic_cast<const CharRE *>(dynamic_cast<PlusRE *>(k.get())->getKid(0).get()));
	// 	    }))
		paths = findAllMatchingPathsOpt(rNFA, nfa);
	// else
		// paths = findAllMatchingPaths(rNFA, nfa);

	std::vector<NFA::State *> inits(lNFA.start_begin(), lNFA.start_end());
	std::vector<NFA::State *> finals(lNFA.accept_begin(), lNFA.accept_end());

	lNFA.clearAllAccepting();
	lNFA.clearAllStarting();

	std::for_each(paths.begin(), paths.end(), [&](auto &ps){
		std::unordered_map<NFA::State *, NFA::State *> m;

		auto lcopy = lNFA.copy(&m);
		nfa.alt(std::move(lcopy));

		std::for_each(ps.begin(), ps.end(), [&](auto &p){
			std::for_each(inits.begin(), inits.end(), [&](auto *i){
				nfa.addEpsilonTransitionSucc(p.start, m[i]);
			});
			std::for_each(finals.begin(), finals.end(), [&](auto *f){
				nfa.addEpsilonTransitionPred(m[f], p.end);
			});
		});
	});
}

void pruneNFA(NFA &nfa, const NFA &other)
{
	std::vector<Relation> orels;
	std::vector<PredicateSet> opreds;
	std::for_each(other.states_begin(), other.states_end(), [&](auto &s){
		std::for_each(s->out_begin(), s->out_end(), [&](auto &t){
			if (!t.label.isPredicate()) {
				orels.push_back(*t.label.getRelation());
			} else {
				opreds.push_back(t.label.getPreChecks());
			}
		});
	});

	std::sort(opreds.begin(), opreds.end());
	opreds.erase(std::unique(opreds.begin(), opreds.end()), opreds.end());
	std::sort(orels.begin(), orels.end());
	orels.erase(std::unique(orels.begin(), orels.end()), orels.end());

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s){
		nfa.removeTransitionsIf(&*s, [&](auto &t){
			return (t.label.isRelation() &&
				std::none_of(orels.begin(), orels.end(),
					     [&](auto &r){ return t.label.getRelation()->includes(r); })) ||
				(t.label.isPredicate() &&
				 std::none_of(opreds.begin(), opreds.end(),
					      [&](auto &p){ return t.label.getPreChecks().includes(p); }));
		});
	});
}

void removeConsecutivePredicates(NFA &nfa)
{
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s){
		nfa.removeTransitionsIf(&*s, [&](auto &t){
			return t.dest == &*s && t.label.isPredicate();
		});
	});

	/* Collect states w/ incoming both preds + rels */
	std::vector<NFA::State *> toDuplicate;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s){
		if (std::any_of(s->in_begin(), s->in_end(), [&](auto &t){
					return t.label.isPredicate();
				}) &&
			std::any_of(s->in_begin(), s->in_end(), [&](auto &t){
					return !t.label.isPredicate();
				})) {
			toDuplicate.push_back(&*s);
}
	});

	std::for_each(toDuplicate.begin(), toDuplicate.end(), [&](auto &s){
		nfa.splitState(s, [&](auto &t){ return t.label.isPredicate(); });
	});

	nfa.addTransitivePredicateEdges();

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s){
		nfa.removeTransitionsIf(&*s, [&](auto &t){
			return t.dest == &*s && t.label.isPredicate();
		});
	});
}

void normalize(NFA &nfa, Constraint::ValidFunT vfun)
{
	nfa.simplify(std::move(vfun));
	saturateDomains(nfa);
	nfa.breakToParts();
	removeConsecutivePredicates(nfa);
	nfa.removeDeadStates();
}

auto checkInclusion(const RegExp *re1, const RegExp *re2,
		    const std::vector<std::unique_ptr<Constraint>> &assms,
		    Counterexample &cex, Constraint::ValidFunT vfun,
		    bool satInitFinalPreds = false) -> bool
{
	auto nfa1 = re1->toNFA();
	normalize(nfa1, vfun);
	if (satInitFinalPreds) {
		saturateInitFinalPreds(nfa1);
		saturateLoc(nfa1);
	}
	auto lhs = nfa1.to_DFA().first;

	auto nfa2 = re2->toNFA();
	normalize(nfa2, vfun);
	if (!assms.empty()) {
		std::for_each(assms.begin(), assms.end(), [&](auto &assm){
			expandAssumption(nfa2, &*assm, nfa1);
			normalize(nfa2, vfun);
		});
	}
	pruneNFA(nfa2, nfa1);
	nfa2.removeDeadStates();
	normalize(nfa2, vfun);
	return lhs.isDFASubLanguageOfNFA(nfa2, cex, vfun);
}

auto Kater::checkAssertion(Constraint *c, Counterexample &cex) -> bool
{
	if (getConf().verbose >= 2) {
		std::cout << "Checking assertion " << *c << std::endl;
	}

	for (int i = 0; i < c->getNumKids(); i++) {
		expandSavedVars(c->getKid(i));
		expandRfs(c->getKid(i));
	}

	auto result = true;
	auto visitor = make_visitor(
		type_list<SubsetConstraint, SubsetSameEndsConstraint>{},
		[&](const SubsetConstraint & /*sc*/){
			result = checkInclusion(&*c->getKid(0), &*c->getKid(1),
						module->getAssumes(), cex,
						[&](auto & /*t*/){ return true; });
		},
		[&](const SubsetSameEndsConstraint & /*sc*/){
			result = checkInclusion(&*c->getKid(0), &*c->getKid(1),
						module->getAssumes(), cex,
						[&](auto & /*t*/){ return true; }, true);

		});
	visitor(*c);
	return result;
}

auto Kater::checkAssertions() -> bool
{
	registerDefaultAssumptions();

	bool status = true;
	std::for_each(module->assert_begin(), module->assert_end(), [&](auto &p){
		Counterexample cex;
		if (!checkAssertion(&*p.co, cex)) {
			std::cerr << p.loc << ": [Error] Assertion does not hold." << std::endl;
			printCounterexample(cex);
			status = false;
		}
	});
	return status;
}

auto Kater::checkExportRequirements() -> bool
{
	auto &module = getModule();

	/* Ensure that pporf is implied by the acyclicity constraints */
	auto ppo = module.getPPO();
	if (!ppo) {
		std::cerr << "[Error] No top-level ppo definition provided\n";
		return false;
	}
	auto hb = module.getHB();
	if (!hb) {
		std::cerr << "[Error] No top-level hb_stable definition provided\n";
		return false;
	}
	auto *hbRE = dynamic_cast<CharRE *>(&*hb);
	if ((hbRE == nullptr) || !hbRE->getLabel().getRelation()) {
		std::cerr << "[Error] hb_stable needs to be stored in a view\n";
		return false;
	}
	auto hbIt = std::find_if(module.svar_begin(), module.svar_end(), [&](auto &kv){
		return kv.first == *hbRE->getLabel().getRelation() && kv.second.status == VarStatus::View;
	});
	if (hbIt == module.svar_end()) {
		std::cerr << "[Error] hb_stable needs to be stored in a view\n";
		return false;
	}

	auto pporf = module.getPPORF();
	auto acycDisj = std::accumulate(module.acyc_begin(), module.acyc_end(),
					RegExp::createFalse(), [&](URE &re1, URE &re2){
						return AltRE::createOpt(re1->clone(), re2->clone());
					});
	bool status = true;
	Counterexample cex;
	auto noOOTA = SubsetConstraint::create(pporf->clone(), StarRE::createOpt(std::move(acycDisj)));
	if (!checkAssertion(&*noOOTA, cex)) {
		std::cerr << "[Warning] Acyclicity constraints do not preclude OOTA.\n";
		std::cerr << "OOTA needs to be enforced by the model checker\n";
		printCounterexample(cex);
	}

	/* Ensure that all saved relations are included in pporf;ppo and are transitive */
	std::for_each(module.svar_begin(), module.svar_end(), [&](auto &kv){
			auto &sv = kv.second;
			Counterexample cex;
			if (sv.status != VarStatus::View) {
				auto savedInPO = SubsetConstraint::create(
					sv.exp->clone(), StarRE::createOpt(
						SeqRE::createOpt(StarRE::createOpt(pporf->clone()), ppo->clone())));
				if (!checkAssertion(&*savedInPO, cex)) {
					std::cerr << "[Error] Saved relation not included in pporf;ppo: " << *sv.exp << "\n";
					printCounterexample(cex);
					status = false;
				}
			} else {
				auto savedInPO = SubsetConstraint::create(
					sv.exp->clone(), StarRE::createOpt(
						SeqRE::createOpt(StarRE::createOpt(module.getPORF()->clone()),
								 module.getRegisteredID("po"))));
				if (!checkAssertion(&*savedInPO, cex)) {
					std::cerr << "[Error] View not included in porf;po: " << *sv.exp << "\n";
					printCounterexample(cex);
					status = false;
				}
			}

			if (sv.status != VarStatus::Normal) {
				cex.clear();
				auto prefix = (sv.status == VarStatus::Reduce) ? sv.red->clone() : ppo->clone();
				auto seqExp = SeqRE::createOpt(std::move(prefix), sv.exp->clone());
				auto savedTrans = SubsetConstraint::createOpt(std::move(seqExp), sv.exp->clone());
				if (!checkAssertion(&*savedTrans, cex)) {
					std::cerr << "[Error] Reduced relation not transitive: " << *sv.exp << "\n";
					printCounterexample(cex);
					status = false;
				}
			}
	});

	/* Ensure that only one coherence constraint has been given, and that it involves a view */
	if (module.getCohNum() != 1) {
		std::cerr << "[Error] Only one coherence constraint is supported\n";
		return false;
	}
	auto *coh = dynamic_cast<CharRE *>(&**module.coh_begin());
	if ((coh == nullptr) || !coh->getLabel().getRelation()) {
		std::cerr << "[Error] Coherence constraint needs to take a view argument\n";
		return false; /* skip the rest of the checks */
	}
	auto vIt = std::find_if(module.svar_begin(), module.svar_end(), [&](auto &kv){
		return kv.first == *coh->getLabel().getRelation() && kv.second.status == VarStatus::View;
	});
	if (vIt == module.svar_end()) {
		std::cerr << "[Error] Coherence constraint needs to take a view argument\n";
		status = false;
	}
	return status;
}

void transitivizeSaved(KatModule &module, URE &r)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		transitivizeSaved(module, r->getKid(i));
	}
	if (auto *re = dynamic_cast<CharRE *>(&*r)) {
		if (re->getLabel().isBuiltin()) {
			return;
		}
		auto sIt = std::find_if(module.svar_begin(), module.svar_end(),
					[&](auto &p){ return p.first == *re->getLabel().getRelation(); });
		if (sIt == module.svar_end() || sIt->second.status == VarStatus::Normal) {
			return;
		}
		r = PlusRE::createOpt(re->clone());
	}
}

void Kater::generateNFAs()
{
	auto &module = getModule();
	auto &cnfas = getCNFAs();
	auto ppo = module.getPPO();

	auto isValidLabel = [&](auto & /*lab*/){ return true; };

	auto i = 0U;
	std::for_each(module.svar_begin(), module.svar_end(), [&](auto &kv){
		auto &v = kv.second;
		NFA n = v.exp->toNFA();

		// FIXME: Use polymorphism
		if (v.status == VarStatus::Reduce) {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for reduce[" << i << "] = "
					  << *v.exp << std::endl;
			}

			auto *pRE = dynamic_cast<PlusRE *>(&*v.exp);
			assert(pRE); // FIXME: currently we implicitly assume this (see transitivize); maybe tip?
			assert(v.red);

			n = pRE->getKid(0)->toNFA();

			n.simplify(isValidLabel).reduce(v.redT);

			NFA rn = v.red->toNFA();
			rn.star().simplify(isValidLabel).seq(std::move(n)).simplify(isValidLabel);

			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for reduce[" << i << "]: " << rn << std::endl;
			}

			cnfas.addReduced(std::move(rn));
		} else if (v.status == VarStatus::View) {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for view[" << i << "] = " << *v.exp << std::endl;
			}

			auto *pRE = dynamic_cast<PlusRE *>(&*v.exp);
			assert(pRE); // FIXME: currently we implicitly assume this (see transitivize); maybe tip?

			n = pRE->getKid(0)->toNFA();
			n.simplify(isValidLabel);

			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for view[" << i << "]: " << n << std::endl;
			}
			cnfas.addView(std::move(n));

			if (kv.first == dynamic_cast<CharRE *>(&**module.coh_begin())->getLabel().getRelation()) {
				cnfas.setCohIndex(i);
			}
			if (kv.first == dynamic_cast<CharRE *>(&*module.getHB())->getLabel().getRelation()) {
				cnfas.setHbIndex(i);
			}
		} else {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for save[" << i << "] = "
					  << *v.exp << std::endl;
			}
			n.simplify(isValidLabel);
			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for save[" << i << "]: " << n << std::endl;
			}
			cnfas.addSaved(std::move(n));
		}
		++i;
	});

	std::for_each(module.acyc_begin(), module.acyc_end(), [&](auto &r){
		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for acyclic " << *r << std::endl;
		}
		// Covert the regural expression to an NFA
		auto tr = r->clone();
		transitivizeSaved(module, tr);
		NFA n = tr->toNFA();
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		n.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA: " << n << std::endl;
		}
		// Simplify the NFA
		n.simplify(isValidLabel);
		if (getConf().verbose >= 3 && module.getAcyclicNum() > 1) {
			std::cout << "Generated NFA: " << n << std::endl;
		}
		cnfas.addAcyclic(std::move(n));
	});
	if (getConf().verbose >= 3) {
		std::cout << "Generated NFA: " << cnfas.getAcyclic() << std::endl;
	}

	std::for_each(module.incl_begin(), module.incl_end(), [&](auto &r){
		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for inclusion " << *r.lhs << " <= " << *r.rhs << std::endl;
		}

		// Covert the regural expression to an NFA
		auto tl = r.lhs->clone();
		transitivizeSaved(module, tl);
		auto lhs = tl->toNFA();
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		// lhs.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA (LHS): " << lhs << std::endl;
		}
		// Simplify the NFA
		lhs.simplify(isValidLabel);
		if (getConf().verbose >= 3) {
			std::cout << "Generated NFA (LHS): " << lhs << std::endl;
		}

		// Covert the regural expression to an NFA
		auto tr = r.rhs->clone();
		transitivizeSaved(module, tr);
		auto rhs = tr->toNFA();
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		// rhs.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA (RHS): " << rhs << std::endl;
		}
		// Simplify the NFA
		rhs.simplify(isValidLabel);
		if (getConf().verbose >= 3) {
			std::cout << "Generated NFA (RHS): " << rhs << std::endl;
		}

		auto j = -1;
		auto *rhsRE = dynamic_cast<CharRE *>(&*r.rhs);
		if (rhsRE && rhsRE->getLabel().getRelation()) {
			for (auto sIt = module.svar_begin(), sE = module.svar_end(); sIt != sE; ++sIt) {
				if (sIt->second.status == VarStatus::View && sIt->first == rhsRE->getLabel().getRelation()) {
					j = std::distance(module.svar_begin(), sIt);
				}
			}
		}
		cnfas.addInclusion({Inclusion<NFA>(std::move(lhs), std::move(rhs), r.type, r.s), j});
	});

	NFA rec;
	std::for_each(module.rec_begin(), module.rec_end(), [&](auto &r){
		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for recovery " << *r << std::endl;
		}
		// Covert the regural expression to an NFA
		auto tr = r->clone();
		transitivizeSaved(module, tr);
		NFA n = tr->toNFA();
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		if (getConf().verbose >= 4) {
			std::cout << "Non-star, non-simplified rec NFA: " << n << std::endl;
		}
		n.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified rec NFA: " << n << std::endl;
		}
		// Simplify the NFA
		n.simplify(isValidLabel);
		if (getConf().verbose >= 3) {
			std::cout << "Generated rec NFA: " << n << std::endl;
		}
		rec.alt(std::move(n));
	});
	if (module.getRecoveryNum() != 0u) {
		auto rf = module.getRegisteredID("rf");
		auto recov = module.getRegisteredID("REC");
		auto po = module.getRegisteredID("po");
		auto fr = module.getRegisteredID("fr");
		auto poInv = module.getRegisteredID("po");
		poInv->flip();

		auto rfRecovPoFr = SeqRE::createOpt(rf->clone(), recov->clone(),
						    po->clone(), fr->clone());

		auto rfRecovPoInvFr = SeqRE::createOpt(rf->clone(), recov->clone(),
						       poInv->clone(), fr->clone());

		rec.alt(rfRecovPoFr->toNFA());
		rec.alt(rfRecovPoInvFr->toNFA());

		rec.star();

		if (getConf().verbose >= 3) {
			std::cout << "Generated full rec NFA: " << rec << std::endl;
		}

		rec.simplify(isValidLabel);

		cnfas.addRecovery(std::move(rec));
		if (getConf().verbose >= 3) {
			std::cout << "Generated full rec NFA simplified: " << cnfas.getRecovery() << std::endl;
		}
	}

	auto pporfNFA = module.getPPORF()->toNFA();
	pporfNFA.simplify(isValidLabel);
	cnfas.addPPoRf(std::move(pporfNFA), module.isDepTracking());
	if (getConf().verbose >= 3) {
		std::cout << "Generated pporf NFA simplified: " << cnfas.getPPoRf().first << std::endl;
	}

	cnfas.setDepTracking(module.isDepTracking());
}

auto Kater::exportCode(std::string &dirPrefix, std::string &outPrefix) -> bool
{
	if (!checkExportRequirements()) {
		return false;
	}

	generateNFAs();

	Printer p(dirPrefix, outPrefix);
	p.output(getCNFAs());
	return true;
}
