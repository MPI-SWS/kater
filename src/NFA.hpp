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

#ifndef KATER_NFA_HPP
#define KATER_NFA_HPP

#include "Counterexample.hpp"
#include "TransLabel.hpp"
#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

class NFA {

public:
	enum class ReductionType { Po, Self };


	class State;

	/* There are no heterogenous lookups before C++20,
	 * so let's just use a UPs with a custom deleter  */
	template<class T>
	struct maybe_deleter{
		bool _delete;
		explicit maybe_deleter(bool doit = true) : _delete(doit) {}

		void operator()(T* p) const{
			if(_delete) { delete p; }
		}
	};

	template<class T>
	using set_unique_ptr = std::unique_ptr<T, maybe_deleter<T>>;

	template<class T>
	auto make_find_ptr(T* raw) -> set_unique_ptr<T>{
		return set_unique_ptr<T>(raw, maybe_deleter<T>(false));
	}

	using StateUPVectorT = std::vector<set_unique_ptr<State>>;
	using StateVectorT = std::vector<State *>;

	/*
	 * A struct representing NFA transitions
	 */
	struct Transition {
		TransLabel label;
		State *dest;

		Transition() = delete;
		Transition(const TransLabel &lab, State *dest)
			: label(lab), dest(dest) {}

		auto copyTo(State *s) const -> Transition {
			auto l = label;
			return Transition(l, s);
		}

		/* Returns a transition with the label flipped,
		 * and the destination changed to DEST */
		auto flipTo(State *s) const -> Transition {
			auto l = label;
			l.flip();
			return Transition(l, s);
		}

		inline auto operator==(const Transition &t) const -> bool {
			return dest == t.dest && label == t.label;
		}
		inline auto operator!=(const Transition &t) const -> bool {
			return !(*this == t);
		}

		inline auto operator<(const Transition &t) const -> bool {
			return (dest < t.dest) || (dest == t.dest && label < t.label);
		}
		inline auto operator>=(const Transition &t) const -> bool {
			return !(*this < t);
		}
		inline auto operator>(const Transition &t) const -> bool {
			return (*this >= t) && (*this != t);
		}
		inline auto operator<=(const Transition &t) const -> bool {
			return !(*this > t);
		}
	};


	/*
	 * A class representing NFA states.
	 * Each state maintains a collections of its transitions.
	 */
	class State {

	private:
		/* FIXME: Do not use set */
		using TransitionsC = std::set<Transition>;

	public:
		State() = delete;
		State(unsigned id) : id(id), starting(false), accepting(false) {}

		using trans_iterator = TransitionsC::iterator;
		using trans_const_iterator = TransitionsC::const_iterator;

		auto out_begin() -> trans_iterator { return getOutgoing().begin(); }
		auto out_end() -> trans_iterator { return getOutgoing().end(); }

		[[nodiscard]] auto out_begin() const -> trans_const_iterator { return getOutgoing().begin(); }
		[[nodiscard]] auto out_end() const -> trans_const_iterator { return getOutgoing().end(); }

		auto in_begin() -> trans_iterator { return getIncoming().begin(); }
		auto in_end() -> trans_iterator { return getIncoming().end(); }

		[[nodiscard]] auto in_begin() const -> trans_const_iterator { return getIncoming().begin(); }
		[[nodiscard]] auto in_end() const -> trans_const_iterator { return getIncoming().end(); }

		/* Returns this state's ID */
		[[nodiscard]] auto getId() const -> unsigned { return id; }

		/* Whether this state is a starting state of the NFA */
		[[nodiscard]] auto isStarting() const -> bool { return starting; }

		/* Whether this state is an accepting state of the NFA */
		[[nodiscard]] auto isAccepting() const -> bool { return accepting; }

		void setStarting(bool b) { starting = b; }
		void setAccepting(bool b) { accepting = b; }

		/* Whether this state has an outgoing transition T */
		[[nodiscard]] auto hasOutgoing(const Transition &t) const -> bool {
			return getOutgoing().count(t) != 0U;
		}

		/* Whether this state has any outgoing transitions */
		[[nodiscard]] auto hasOutgoing() const -> bool { return out_begin() != out_end(); }

		[[nodiscard]] auto getNumOutgoing() const -> size_t { return getOutgoing().size(); }

		/* Whether this state has an inverse transition T */
		[[nodiscard]] auto hasIncoming(const Transition &t) const -> bool {
			return getIncoming().count(t) != 0U;
		}

		/* Whether this state has any incoming transitions */
		[[nodiscard]] auto hasIncoming() const -> bool { return in_begin() != in_end(); }

		[[nodiscard]] auto getNumIncoming() const -> size_t { return getIncoming().size(); }

		auto hasOutgoingTo(State *s) const -> bool {
			return std::any_of(out_begin(), out_end(), [&](const Transition &t){
					return t.dest == s;
				});
		}

		auto hasIncomingTo(State *s) const -> bool {
			return std::any_of(in_begin(), in_end(), [&](const Transition &t){
					return t.dest == s;
				});
		}

		/* Adds an outgoing transition T (if it doesn't exist) */
		void addOutgoing(const Transition &t) {
			getOutgoing().insert(t);
		}

		/* Adds an incoming transition T (if it doesn't exist) */
		void addIncoming(const Transition &t) {
			getIncoming().insert(t);
		}

		/* Removes outgoing transition T from this state (if exists) */
		void removeOutgoing(const Transition &t) {
			getOutgoing().erase(t);
		}

		/* Removes incoming transition T from this state (if exists) */
		void removeIncoming(const Transition &t) {
			getIncoming().erase(t);
		}

		/* Removes all outgoing with S as their dest */
		void removeOutgoingTo(State *s) {
			for (auto it = out_begin(), ie = out_end(); it != ie; /* */) {
				if (it->dest == s) {
					it = getOutgoing().erase(it);
				} else {
					++it;
				}
			}
		}

		/* Removes all incoming with S as their dest */
		void removeIncomingTo(State *s) {
			for (auto it = in_begin(), ie = in_end(); it != ie; /* */) {
				if (it->dest == s) {
					it = getIncoming().erase(it);
				} else {
					++it;
				}
			}
		}

		/* Returns true if all outgoing transitions of this
		 * state return to this state */
		[[nodiscard]] auto hasAllOutLoops() const -> bool {
			return std::all_of(out_begin(), out_end(), [&](const Transition &t){
				return t.dest == this;
			});
		}

		/* Returns true if all incoming transition of this
		 * state return to this state */
		[[nodiscard]] auto hasAllInLoops() const -> bool {
			return std::all_of(in_begin(), in_end(), [&](const Transition &t){
				return t.dest == this;
			});
		}

		[[nodiscard]] auto hasAllOutPredicates() const -> bool {
			return std::all_of(out_begin(), out_end(), [](auto &t){
				return t.label.isPredicate();
			});
		}

		[[nodiscard]] auto hasAllInPredicates() const -> bool {
			return std::all_of(in_begin(), in_end(), [](auto &t){
				return t.label.isPredicate();
			});
		}

		/* Whether all outgoing transitions are the same as the ones of S */
		auto outgoingSameAs(State *s) const -> bool {
			return getOutgoing() == s->getOutgoing();
		}

		/* Whether all incoming transitions are the same as the ones of S */
		auto incomingSameAs(State *s) const -> bool {
			return getIncoming() == s->getIncoming();
		}

		auto flip() -> State & {
			std::swap(getOutgoing(), getIncoming());
			std::swap(starting, accepting);
			return *this;
		}

	private:
		[[nodiscard]] auto getOutgoing() const -> const TransitionsC & { return transitions; }
		auto getOutgoing() -> TransitionsC & { return transitions; }

		[[nodiscard]] auto getIncoming() const -> const TransitionsC & { return inverse; }
		auto getIncoming() -> TransitionsC & { return inverse; }

		unsigned id;
		bool starting;
		bool accepting;
		TransitionsC transitions;
		TransitionsC inverse;
	};


	NFA() = default;
	NFA(const TransLabel &label);

	using stateUP_iterator = StateUPVectorT::iterator;
	using stateUP_const_iterator = StateUPVectorT::const_iterator;

	auto states_begin() -> stateUP_iterator { return getStates().begin(); }
	auto states_end() -> stateUP_iterator { return getStates().end(); }

	[[nodiscard]] auto states_begin() const -> stateUP_const_iterator { return getStates().begin(); }
	[[nodiscard]] auto states_end() const -> stateUP_const_iterator { return getStates().end(); }

	using state_iterator = StateVectorT::iterator;
	using state_const_iterator = StateVectorT::const_iterator;

	auto start_begin() -> state_iterator { return getStarting().begin(); }
	auto start_end() -> state_iterator { return getStarting().end(); }

	[[nodiscard]] auto start_begin() const -> state_const_iterator { return getStarting().begin(); }
	[[nodiscard]] auto start_end() const -> state_const_iterator { return getStarting().end(); }

	auto accept_begin() -> state_iterator { return getAccepting().begin(); }
	auto accept_end() -> state_iterator { return getAccepting().end(); }

	[[nodiscard]] auto accept_begin() const -> state_const_iterator { return getAccepting().begin(); }
	[[nodiscard]] auto accept_end() const -> state_const_iterator { return getAccepting().end(); }

	[[nodiscard]] auto getNumStates() const -> unsigned { return getStates().size(); }

	[[nodiscard]] auto getNumStarting() const -> unsigned { return getStarting().size(); }

	[[nodiscard]] auto getNumAccepting() const -> unsigned { return getAccepting().size(); }

	static auto isStarting(State *state) -> bool { return state->isStarting(); }

	static auto isAccepting(State *state) -> bool { return state->isAccepting(); }

	/* Creates and adds a new (unreachable) state to the NFA and its inverse.
	 * Returns the newly added state */
	auto createState() -> State * {
		static unsigned counter = 0;
		return getStates().emplace_back(new State(counter++)).get();
	}

	auto createStarting() -> State * {
		auto *s = createState();
		makeStarting(s);
		return s;
	}

	auto createAccepting() -> State * {
		auto *s = createState();
		makeAccepting(s);
		return s;
	}

	void makeStarting(State *s) {
		if (!isStarting(s)) {
			s->setStarting(true);
			getStarting().push_back(s);
		}
	}

	void makeAccepting(State *s) {
		if (!isAccepting(s)) {
			s->setAccepting(true);
			getAccepting().push_back(s);
		}
	}

	void clearStarting(State *s) {
		s->setStarting(false);
		auto &starting = getStarting();
		starting.erase(std::remove_if(starting.begin(), starting.end(),
					      [s](auto &si){ return &*si == s; }),
			       starting.end());
	}

	void clearAllStarting() {
		std::for_each(start_begin(), start_end(), [&](auto &s){
			s->setStarting(false);
		});
		getStarting().clear();
	}

	void clearAccepting(State *s) {
		s->setAccepting(false);
		auto &accept = getAccepting();
		accept.erase(std::remove_if(accept.begin(), accept.end(),
					    [s](auto &si){ return &*si == s; }),
			     accept.end());
	}

	void clearAllAccepting() {
		std::for_each(accept_begin(), accept_end(), [&](auto &s){
			s->setAccepting(false);
		});
		getAccepting().clear();
	}

	auto removeState(stateUP_iterator &it) -> stateUP_iterator {
		std::for_each(states_begin(), states_end(), [&](decltype(*states_begin()) &s){
			s->removeOutgoingTo(it->get());
			s->removeIncomingTo(it->get());
		});
		auto sit = std::find(getStarting().begin(), getStarting().end(), it->get());
		if (sit != getStarting().end()) {
			getStarting().erase(sit);
		}
		auto ait = std::find(getAccepting().begin(), getAccepting().end(), it->get());
		if (ait != getAccepting().end()) {
			getAccepting().erase(ait);
		}
		return getStates().erase(it);
	}

	/* Removes STATE from the NFA and its inverse */
	void removeState(State *state) {
		auto it = std::find(states_begin(), states_end(), make_find_ptr(state));
		if (it != states_end()) {
			removeState(it);
		}
	}

	template<typename ITER>
	void removeStates(ITER &&begin, ITER &&end) {
		std::for_each(begin, end, [&](State *s){ removeState(s); });
	}

	/* Creates a copy D of S such that S only keeps (incoming)
	 * transitions that satisfy FUN (and D the rest) */
	template<typename F>
	auto splitState(State *s, F&& fun) -> State * {
		auto *d = createState();
		if (isAccepting(s)) {
			makeAccepting(d);
		}
		addTransitions(d, s->out_begin(), s->out_end());
		addInvertedTransitions(d, s->in_begin(), s->in_end());
		removeInvertedTransitionsIf(d, [&](auto &t){ return fun(t); });
		removeInvertedTransitionsIf(s, [&](auto &t){ return !fun(t); });
		return d;
	}

	/* Whether the (regular) NFA has a transition T from state SRC */
	static auto hasTransition(State *src, const Transition &t) -> bool {
		return src->hasOutgoing(t);
	}

	/* Adds a transition to the NFA and updates the inverse */
	static void addTransition(State *src, const Transition &t) {
		src->addOutgoing(t);
		t.dest->addIncoming(t.flipTo(src));
	}

	template<typename ITER>
	void addTransitions(State *src, ITER &&begin, ITER &&end) {
		std::for_each(begin, end, [&](const Transition &t){
			addTransition(src, t);
		});
	}

	static void addInvertedTransition(State *dst, const Transition &t) {
		addTransition(t.dest, t.flipTo(dst));
	}

	template<typename ITER>
	void addInvertedTransitions(State *dst, ITER &&begin, ITER &&end) {
		std::for_each(begin, end, [&](const Transition &t){
			addInvertedTransition(dst, t);
		});
	}

	static void addSelfTransition(State *src, const TransLabel &lab) {
		addTransition(src, Transition(lab, src));
	}

	/*
	 * Adds an ε transition from SRC to DST by connecting SRC to
	 * all of DST's successors (using the respective labels).
	 */
	void addEpsilonTransitionSucc(State *src, State *dst) {
		addTransitions(src, dst->out_begin(), dst->out_end());
		if (isAccepting(dst)) {
			makeAccepting(src);
		}
		if (isStarting(src)) {
			makeStarting(dst);
		}
	}

	/*
	 * Adds an ε transition from SRC to DST by connecting all
	 * of SRC's predecessors to DST (using the respective labels).
	 */
	void addEpsilonTransitionPred(State *src, State *dst) {
		addInvertedTransitions(dst, src->in_begin(), src->in_end());
		if (isAccepting(dst)) {
			makeAccepting(src);
		}
		if (isStarting(src)) {
			makeStarting(dst);
		}
	}

	auto addTransitionToFresh(State *src, const TransLabel &lab) -> State * {
		auto *dst = createState();
		addTransition(src, Transition(lab, dst));
		return dst;
	}

	/* Removes a transition T from S and updates the inverse */
	static void removeTransition(State *src, const Transition &t) {
		src->removeOutgoing(t);
		t.dest->removeIncoming(t.flipTo(src));
	}

	template<typename ITER>
	void removeTransitions(State *src, ITER &&begin, ITER &&end){
		std::for_each(begin, end, [&](const Transition &t){
			removeTransition(src, t);
		});
	}

	/* Remove all of SRC's transitions that satisfy PRED */
	template<typename F>
	void removeTransitionsIf(State *src, F&& pred) {
		std::vector<Transition> toRemove;
		std::copy_if(src->out_begin(), src->out_end(), std::back_inserter(toRemove),
			     [&](const Transition &t){ return pred(t); });
		removeTransitions(src, toRemove.begin(), toRemove.end());
	}

	template<typename ITER>
	void removeInvertedTransitions(State *dst, ITER &&begin, ITER &&end){
		std::for_each(begin, end, [&](const Transition &t){
			removeTransition(t.dest, t.flipTo(dst));
		});
	}

	template<typename F>
	void removeInvertedTransitionsIf(State *dst, F&& pred) {
		std::vector<Transition> toRemove;
		std::copy_if(dst->in_begin(), dst->in_end(), std::back_inserter(toRemove),
			     [&](const auto &t){ return pred(t); });
		removeInvertedTransitions(dst, toRemove.begin(), toRemove.end());
	}

	void removeAllTransitions(State *src) {
		removeTransitionsIf(src, [&](const Transition & /*t*/){ return true; });
	}

	auto flip() -> NFA &;
	auto alt(NFA &&other) -> NFA &;
	auto seq(NFA &&other) -> NFA &;
	auto or_empty() -> NFA &;
	auto star() -> NFA &;
	auto plus() -> NFA &;

	auto simplify(std::function<bool(const TransLabel &)> isValidTransition =
		      [](const TransLabel & /*lab*/){ return true; }) -> NFA &;
	auto reduce(ReductionType t) -> NFA &;

	auto removeDeadStates() -> NFA &;

	auto addTransitivePredicateEdges(bool removeOld = true) -> NFA &;

	auto breakToParts() -> NFA &;

	template<typename F>
	void foreachPathReachableFrom(const std::vector<State *> &ss, F &&fun) const {
		std::unordered_set<State *> visited;
		std::vector<State *> workList;

		for (auto *s : ss) {
			visited.insert(s);
			workList.push_back(s);
		}
		while (!workList.empty()) {
			auto *s = workList.back();
			workList.pop_back();
			for (auto it = s->out_begin(); it != s->out_end(); it++) {
				fun({s, *it});
				if (visited.count(it->dest)) {
					continue;
				}
				visited.insert(it->dest);
				workList.push_back(it->dest);
			}
		}
	}

	template<typename F>
	void foreachPathReachingTo(const std::vector<State *> &ss, F &&fun) {
		flip();
		foreachPathReachableFrom(ss, [&](std::pair<State *, Transition> p){
			/* Interpret the results of foreach as pairs, invert,
			 * and then feed them to whatever FUN expects */
			return fun({p.second.dest, p.second.flipTo(p.first)});
		});
		flip();
	}


	[[nodiscard]] static auto
	calculateReachableFrom(const std::vector<State *> &ss) -> std::unordered_set<State *>;
	auto
	calculateReachingTo(const std::vector<State *> &ss) -> std::unordered_set<State *>;

	[[nodiscard]] auto acceptsEmptyString() const -> bool;
	auto acceptsNoString(Counterexample &cex) const -> bool;

	auto isDFASubLanguageOfNFA(const NFA &other, Counterexample &cex,
				const std::function<bool(const TransLabel &)>& isValidTransition =
				[](const TransLabel & /*lab*/){ return true; }) const -> bool;

	auto isSubLanguageOfDFA(const NFA &other, Counterexample &cex,
				const std::function<bool(const TransLabel &)>& isValidTransition =
				[](const TransLabel & /*lab*/){ return true; }) const -> bool;

	[[nodiscard]] auto to_DFA () const -> std::pair<NFA, std::map<State *, std::set<State *>>>;

	auto copy(std::unordered_map<State *, State *> *mapping = nullptr) const -> NFA;

	friend auto operator<< (std::ostream& ostr, const NFA& nfa) -> std::ostream&;
	template<typename T>
	friend auto operator<< (std::ostream& ostr, const std::set<T> &s) -> std::ostream &;
	template<typename ITER>
	friend auto assignStateIDs(ITER &&begin, ITER &&end) -> std::unordered_map<NFA::State *, unsigned>;

private:
	template<typename F>
	void applyBidirectionally(F &&fun) {
		fun();
		flip();
		fun();
		flip();
	}

	void breakIntoMultiple(State *s, const Transition &t);

	void simplify_basic();

	auto joinPredicateEdges(std::function<bool(const TransLabel &)> isValidTransition) -> bool;

	void removeRedundantSelfLoops();

	void compactEdges(const std::function<bool(const TransLabel &)>& isValidTransition);

	void removeDeadStatesDFS();

	auto
	calculateUsefulStates() -> std::unordered_set<State *>;

	void scm_reduce ();

	auto
	get_state_composition_matrix () -> std::unordered_map<State *, std::vector<char>>;

	[[nodiscard]] auto
	findSimilarStates() const -> std::unordered_map<State *, std::unordered_map<State *, bool>>;

	void removeSimilarTransitions();

	[[nodiscard]] auto getStates() const -> const StateUPVectorT & { return nfa; }
	auto getStates() -> StateUPVectorT & { return nfa; }

	[[nodiscard]] auto getAccepting() const -> const StateVectorT & { return accepting; }
	auto getAccepting() -> StateVectorT & { return accepting; }

	[[nodiscard]] auto getStarting() const -> const StateVectorT & { return starting; }
	auto getStarting() -> StateVectorT & { return starting; }

	StateUPVectorT nfa;
	StateVectorT starting;
	StateVectorT accepting;
};

#endif /* KATER_NFA_HPP */
