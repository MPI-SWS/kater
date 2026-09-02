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
#include "Error.hpp"
#include "TransLabel.hpp"
#include "VSet.hpp"

#include <algorithm>
#include <cassert>
#include <compare>
#include <cstdint>
#include <map>
#include <memory>
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

class NFA {

public:
	class State;

	using StateUPVectorT = std::vector<std::unique_ptr<State>>;
	using StateVectorT = std::vector<State *>;

	/*
	 * A struct representing NFA transitions
	 */
	struct Transition {
		State *dest;
		TransLabel label;

		Transition() = delete;
		Transition(const TransLabel &lab, State *dest) : label(lab), dest(dest) {}

		auto copyTo(State *s) const -> Transition { return {label, s}; }

		/* Returns a transition with the label flipped,
		 * and the destination changed to DEST */
		auto flipTo(State *s) const -> Transition
		{
			auto l = label;
			l.flip();
			return {l, s};
		}

		/* Order by destination ID, not address: exports must not depend on the
		 * allocator */
		auto operator<=>(const Transition &other) const -> std::strong_ordering
		{
			if (auto cmp = dest->getId() <=> other.dest->getId(); cmp != 0)
				return cmp;
			return label <=> other.label;
		}
		auto operator==(const Transition &other) const -> bool
		{
			return dest == other.dest && label == other.label;
		}
	};

	/*
	 * A class representing NFA states.
	 * Each state maintains a collections of its transitions.
	 */
	class State {

	private:
		/* FIXME: Do not use set */
		using TransitionsC = VSet<Transition>;

	public:
		State() = delete;
		State(unsigned id) : id(id), starting(false), accepting(false) {}

		[[nodiscard]] auto out_begin() const { return getOutgoing().begin(); }
		[[nodiscard]] auto out_end() const { return getOutgoing().end(); }
		[[nodiscard]] auto outs() const { return std::ranges::ref_view(getOutgoing()); }

		[[nodiscard]] auto in_begin() const { return getIncoming().begin(); }
		[[nodiscard]] auto in_end() const { return getIncoming().end(); }
		[[nodiscard]] auto ins() const { return std::ranges::ref_view(getIncoming()); }

		/* Returns this state's ID */
		[[nodiscard]] auto getId() const -> unsigned { return id; }

		/* Whether this state is a starting state of the NFA */
		[[nodiscard]] auto isStarting() const -> bool { return starting; }

		/* Whether this state is an accepting state of the NFA */
		[[nodiscard]] auto isAccepting() const -> bool { return accepting; }

		/* Whether this state has an outgoing transition T */
		[[nodiscard]] auto hasOutgoing(const Transition &t) const -> bool
		{
			return getOutgoing().count(t) != 0U;
		}

		/* Whether this state has any outgoing transitions */
		[[nodiscard]] auto hasOutgoing() const -> bool { return out_begin() != out_end(); }

		[[nodiscard]] auto getNumOutgoing() const -> size_t { return getOutgoing().size(); }

		/* Whether this state has an inverse transition T */
		[[nodiscard]] auto hasIncoming(const Transition &t) const -> bool
		{
			return getIncoming().count(t) != 0U;
		}

		/* Whether this state has any incoming transitions */
		[[nodiscard]] auto hasIncoming() const -> bool { return in_begin() != in_end(); }

		[[nodiscard]] auto getNumIncoming() const -> size_t { return getIncoming().size(); }

		auto hasOutgoingTo(State *s) const -> bool
		{
			return std::any_of(out_begin(), out_end(),
					   [&](const Transition &t) { return t.dest == s; });
		}

		auto hasIncomingTo(State *s) const -> bool
		{
			return std::any_of(in_begin(), in_end(),
					   [&](const Transition &t) { return t.dest == s; });
		}

		/* Returns true if all outgoing transitions of this
		 * state return to this state */
		[[nodiscard]] auto hasAllOutLoops() const -> bool
		{
			return std::all_of(out_begin(), out_end(),
					   [&](const Transition &t) { return t.dest == this; });
		}

		/* Returns true if all incoming transition of this
		 * state return to this state */
		[[nodiscard]] auto hasAllInLoops() const -> bool
		{
			return std::all_of(in_begin(), in_end(),
					   [&](const Transition &t) { return t.dest == this; });
		}

		[[nodiscard]] auto hasAllOutPredicates() const -> bool
		{
			return std::all_of(out_begin(), out_end(),
					   [](auto &t) { return t.label.isPredicate(); });
		}

		[[nodiscard]] auto hasAllInPredicates() const -> bool
		{
			return std::all_of(in_begin(), in_end(),
					   [](auto &t) { return t.label.isPredicate(); });
		}

		/* Whether all outgoing transitions are the same as the ones of S */
		auto outgoingSameAs(State *s) const -> bool
		{
			return getOutgoing() == s->getOutgoing();
		}

		/* Whether all incoming transitions are the same as the ones of S */
		auto incomingSameAs(State *s) const -> bool
		{
			return getIncoming() == s->getIncoming();
		}

		auto flip() -> State &
		{
			std::swap(getOutgoing(), getIncoming());
			std::swap(starting, accepting);
			return *this;
		}

	protected:
		friend NFA;
		void setId(unsigned newId) { id = newId; }
		void setStarting(bool b) { starting = b; }
		void setAccepting(bool b) { accepting = b; }

		/* Adds an outgoing transition T (if it doesn't exist) */
		void addOutgoing(const Transition &t) { getOutgoing().insert(t); }

		/* Adds an incoming transition T (if it doesn't exist) */
		void addIncoming(const Transition &t) { getIncoming().insert(t); }

		/* Removes outgoing transition T from this state (if exists) */
		void removeOutgoing(const Transition &t) { getOutgoing().erase(t); }

		/* Removes incoming transition T from this state (if exists) */
		void removeIncoming(const Transition &t) { getIncoming().erase(t); }

		/* Removes all outgoing with S as their dest */
		void removeOutgoingTo(State *s)
		{
			// FIXME: Add erase_if in VSet
			getOutgoing().erase_if([s](auto &t) { return t.dest == s; });
		}

		template <typename F> void removeOutgoingIf(F &&pred)
		{
			// std::erase_if(getOutgoing(), [&](auto &t){ return pred(t.dest); });
			getOutgoing().erase_if([&](auto &t) { return pred(t.dest); });
		}

		template <typename F> void removeIncomingIf(F &&pred)
		{
			getIncoming().erase_if([&](auto &t) { return pred(t.dest); });
		}

		/* Removes all incoming with S as their dest */
		void removeIncomingTo(State *s)
		{
			getIncoming().erase_if([s](auto &t) { return t.dest == s; });
		}

	private:
		[[nodiscard]] auto getOutgoing() const -> const TransitionsC &
		{
			return transitions;
		}
		auto getOutgoing() -> TransitionsC & { return transitions; }

		[[nodiscard]] auto getIncoming() const -> const TransitionsC & { return inverse; }
		auto getIncoming() -> TransitionsC & { return inverse; }

		/*
		 * Invariant: IDs are dense (position in states vector). Adding/removing
		 * states restores it via compressStateIDs()
		 */
		unsigned id;
		bool starting;
		bool accepting;
		TransitionsC transitions;
		TransitionsC inverse;
	};

	NFA() = default;
	NFA(const TransLabel &label);

	auto states_begin() { return getStates().begin(); }
	auto states_end() { return getStates().end(); }
	auto states() { return std::ranges::ref_view(getStates()); }

	[[nodiscard]] auto states_begin() const { return getStates().begin(); }
	[[nodiscard]] auto states_end() const { return getStates().end(); }
	[[nodiscard]] auto states() const { return std::ranges::ref_view(getStates()); }

	auto start_begin() { return getStarting().begin(); }
	auto start_end() { return getStarting().end(); }
	auto starting() { return std::ranges::ref_view(getStarting()); }

	[[nodiscard]] auto start_begin() const { return getStarting().begin(); }
	[[nodiscard]] auto start_end() const { return getStarting().end(); }
	[[nodiscard]] auto starting() const { return std::ranges::ref_view(getStarting()); }

	auto accept_begin() { return getAccepting().begin(); }
	auto accept_end() { return getAccepting().end(); }
	auto accepting() { return std::ranges::ref_view(getAccepting()); }

	[[nodiscard]] auto accept_begin() const { return getAccepting().begin(); }
	[[nodiscard]] auto accept_end() const { return getAccepting().end(); }
	[[nodiscard]] auto accepting() const { return std::ranges::ref_view(getAccepting()); }

	[[nodiscard]] auto getNumStates() const { return getStates().size(); }

	/* Returns the state with ID (IDs are dense, so this is a plain lookup) */
	[[nodiscard]] auto operator[](unsigned id) const -> State *
	{
		VERIFY(id < getNumStates());
		return getStates()[id].get();
	}

	[[nodiscard]] auto getNumStarting() const { return getStarting().size(); }

	[[nodiscard]] auto getNumAccepting() const { return getAccepting().size(); }

	[[nodiscard]] auto size() const
	{
		auto transitions =
			std::accumulate(states_begin(), states_end(), 0U, [](auto acc, auto &sUP) {
				return acc + sUP->getNumOutgoing();
			});
		return getNumStates() * transitions;
	}

	/* Creates and adds a new (unreachable) state to the NFA and its inverse.
	 * Returns the newly added state */
	auto createState() -> State *
	{
		return getStates().emplace_back(new State(getNumStates())).get();
	}

	auto createStarting() -> State *
	{
		auto *s = createState();
		makeStarting(s);
		return s;
	}

	auto createAccepting() -> State *
	{
		auto *s = createState();
		makeAccepting(s);
		return s;
	}

	void makeStarting(State *s)
	{
		if (!s->isStarting()) {
			s->setStarting(true);
			getStarting().push_back(s);
		}
	}

	void makeAccepting(State *s)
	{
		if (!s->isAccepting()) {
			s->setAccepting(true);
			getAccepting().push_back(s);
		}
	}

	void clearStarting(State *s)
	{
		s->setStarting(false);
		auto &starting = getStarting();
		starting.erase(std::remove_if(starting.begin(), starting.end(),
					      [s](auto &si) { return &*si == s; }),
			       starting.end());
	}

	void clearAllStarting()
	{
		std::for_each(start_begin(), start_end(), [&](auto &s) { s->setStarting(false); });
		getStarting().clear();
	}

	void clearAccepting(State *s)
	{
		s->setAccepting(false);
		auto &accept = getAccepting();
		accept.erase(std::remove_if(accept.begin(), accept.end(),
					    [s](auto &si) { return &*si == s; }),
			     accept.end());
	}

	void clearAllAccepting()
	{
		std::for_each(accept_begin(), accept_end(),
			      [&](auto &s) { s->setAccepting(false); });
		getAccepting().clear();
	}

	template <typename STATE_ITER> auto removeState(STATE_ITER &it) -> STATE_ITER
	{
		std::for_each(states_begin(), states_end(), [&](decltype(*states_begin()) &s) {
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
		auto nextIt = getStates().erase(it);
		compressStateIDs();
		return nextIt;
	}

	/* Removes STATE from the NFA and its inverse */
	void removeState(State *state)
	{
		auto it = states_begin() + state->getId();
		VERIFY(it->get() == state, "a state's ID is its index");
		removeState(it);
	}

	template <typename F> void removeStatesIf(F &&pred)
	{
		for (auto &s : states()) {
			s->removeOutgoingIf(pred);
			s->removeIncomingIf(pred);
		}
		std::erase_if(getStarting(), pred);
		std::erase_if(getAccepting(), pred);
		std::erase_if(getStates(), [&](auto &sUP) { return pred(sUP.get()); });
		compressStateIDs();
	}

	/* Creates a copy D of S such that S only keeps (incoming)
	 * transitions that satisfy FUN (and D the rest) */
	template <typename F> auto splitState(State *s, F &&fun) -> State *
	{
		auto *d = createState();
		if (s->isAccepting()) {
			makeAccepting(d);
		}
		addTransitions(d, s->out_begin(), s->out_end());
		addInvertedTransitions(d, s->in_begin(), s->in_end());
		removeInvertedTransitionsIf(d, [&](auto &t) { return fun(t); });
		removeInvertedTransitionsIf(s, [&](auto &t) { return !fun(t); });
		return d;
	}

	/* Adds a transition to the NFA and updates the inverse */
	static void addTransition(State *src, const Transition &t)
	{
		src->addOutgoing(t);
		t.dest->addIncoming(t.flipTo(src));
	}

	template <typename ITER> void addTransitions(State *src, ITER &&begin, ITER &&end)
	{
		std::for_each(begin, end, [&](const Transition &t) { addTransition(src, t); });
	}

	static void addInvertedTransition(State *dst, const Transition &t)
	{
		addTransition(t.dest, t.flipTo(dst));
	}

	template <typename ITER> void addInvertedTransitions(State *dst, ITER &&begin, ITER &&end)
	{
		std::for_each(begin, end,
			      [&](const Transition &t) { addInvertedTransition(dst, t); });
	}

	static void addSelfTransition(State *src, const TransLabel &lab)
	{
		addTransition(src, Transition(lab, src));
	}

	/* Add an ε transition from SRC to DST (true predicate label) */
	void addEpsilonTransition(State *src, State *dst)
	{
		addTransition(src, Transition(TransLabel(std::nullopt), dst));
		if (dst->isAccepting()) {
			makeAccepting(src);
		}
		if (src->isStarting()) {
			makeStarting(dst);
		}
	}

	/*
	 * Adds an ε transition from SRC to DST by connecting SRC to
	 * all of DST's successors (using the respective labels).
	 */
	void addEpsilonTransitionSucc(State *src, State *dst)
	{
		addTransitions(src, dst->out_begin(), dst->out_end());
		if (dst->isAccepting()) {
			makeAccepting(src);
		}
		if (src->isStarting()) {
			makeStarting(dst);
		}
	}

	/*
	 * Adds an ε transition from SRC to DST by connecting all
	 * of SRC's predecessors to DST (using the respective labels).
	 */
	void addEpsilonTransitionPred(State *src, State *dst)
	{
		addInvertedTransitions(dst, src->in_begin(), src->in_end());
		if (dst->isAccepting()) {
			makeAccepting(src);
		}
		if (src->isStarting()) {
			makeStarting(dst);
		}
	}

	auto addTransitionToFresh(State *src, const TransLabel &lab) -> State *
	{
		auto *dst = createState();
		addTransition(src, Transition(lab, dst));
		return dst;
	}

	/* Removes a transition T from S and updates the inverse */
	static void removeTransition(State *src, const Transition &t)
	{
		src->removeOutgoing(t);
		t.dest->removeIncoming(t.flipTo(src));
	}

	template <typename ITER> void removeTransitions(State *src, ITER &&begin, ITER &&end)
	{
		std::for_each(begin, end, [&](const Transition &t) { removeTransition(src, t); });
	}

	/* Remove all of SRC's transitions that satisfy PRED */
	template <typename F> void removeTransitionsIf(State *src, F &&pred)
	{
		std::vector<Transition> toRemove;
		std::copy_if(src->out_begin(), src->out_end(), std::back_inserter(toRemove),
			     [&](const Transition &t) { return pred(t); });
		removeTransitions(src, toRemove.begin(), toRemove.end());
	}

	template <typename ITER>
	void removeInvertedTransitions(State *dst, ITER &&begin, ITER &&end)
	{
		std::for_each(begin, end, [&](const Transition &t) {
			removeTransition(t.dest, t.flipTo(dst));
		});
	}

	template <typename F> void removeInvertedTransitionsIf(State *dst, F &&pred)
	{
		std::vector<Transition> toRemove;
		std::copy_if(dst->in_begin(), dst->in_end(), std::back_inserter(toRemove),
			     [&](const auto &t) { return pred(t); });
		removeInvertedTransitions(dst, toRemove.begin(), toRemove.end());
	}

	void removeAllTransitions(State *src)
	{
		removeTransitionsIf(src, [&](const Transition & /*t*/) { return true; });
	}

	auto flip() -> NFA &;
	/** Adds OTHER's states after ours, so its state i becomes our
	 * getNumStates()+i (read the count before calling) */
	auto alt(NFA &&other) -> NFA &;
	auto seq(NFA &&other) -> NFA &;
	auto or_empty() -> NFA &;
	auto star() -> NFA &;
	auto plus() -> NFA &;

	template <typename F>
	void foreachPathReachableFrom(const std::vector<State *> &ss, F &&fun) const
	{
		std::vector<uint8_t> visited(getNumStates(), 0);
		std::vector<State *> workList;

		for (auto *s : ss) {
			visited[s->getId()] = 1;
			workList.push_back(s);
		}
		while (!workList.empty()) {
			auto *s = workList.back();
			workList.pop_back();
			for (auto it = s->out_begin(); it != s->out_end(); it++) {
				fun({s, *it});
				if (visited[it->dest->getId()] != 0) {
					continue;
				}
				visited[it->dest->getId()] = 1;
				workList.push_back(it->dest);
			}
		}
	}

	template <typename F> void foreachPathReachingTo(const std::vector<State *> &ss, F &&fun)
	{
		flip();
		foreachPathReachableFrom(ss, [&](std::pair<State *, Transition> p) {
			/* Interpret the results of foreach as pairs, invert,
			 * and then feed them to whatever FUN expects */
			return fun({p.second.dest, p.second.flipTo(p.first)});
		});
		flip();
	}

	[[nodiscard]] auto acceptsEmptyString() const -> bool;
	auto acceptsNoString(Counterexample &cex) const -> bool;

	/* Determinizes via the subset construction. Alongside the DFA, returns the
	 * subset each DFA state stands for, as sorted NFA state IDs indexed by DFA
	 * state ID */
	[[nodiscard]] auto to_DFA() const -> std::pair<NFA, std::vector<std::vector<unsigned>>>;

	friend auto operator<<(std::ostream &ostr, const NFA &nfa) -> std::ostream &;

private:
	void compressStateIDs()
	{
		unsigned id = 0;
		for (auto &s : states()) {
			s->setId(id++);
		}
	}

	[[nodiscard]] auto getStates() const -> const StateUPVectorT & { return nfa; }
	auto getStates() -> StateUPVectorT & { return nfa; }

	[[nodiscard]] auto getAccepting() const -> const StateVectorT & { return accepting_; }
	auto getAccepting() -> StateVectorT & { return accepting_; }

	[[nodiscard]] auto getStarting() const -> const StateVectorT & { return starting_; }
	auto getStarting() -> StateVectorT & { return starting_; }

	StateUPVectorT nfa;
	StateVectorT starting_;
	StateVectorT accepting_;
};

#endif /* KATER_NFA_HPP */
