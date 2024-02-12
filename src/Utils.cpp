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

#include "Utils.hpp"

#include "Error.hpp"
#include "Theory.hpp"

#include <sstream>

auto openFileForWriting(const std::string &filename) -> std::ofstream
{
	std::ofstream fout;

	fout.open(filename, std::ios_base::out);
	if (!fout.is_open()) {
		std::cerr << "Could not open file for writing: " << filename << "\n";
		exit(EPRINT);
	}
	return fout;
}

void printNFAToDot(const NFA &nfa, const std::string &filename, const Theory &theory)
{
	auto fout = openFileForWriting(filename);

	fout << "digraph fsm {\n"
	     << "\trankdir=LR;\n";
	fout << "node [shape = circle]; ";
	std::for_each(nfa.start_begin(), nfa.start_end(), [&](auto &s) {
		fout << s->getId() << "[style=filled, fillcolor = lightgrey] ";
	});
	fout << ";\n";
	std::for_each(nfa.accept_begin(), nfa.accept_end(),
		      [&](auto &s) { fout << s->getId() << "[shape = doublecircle] "; });
	fout << ";\n";
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::for_each(s->out_begin(), s->out_end(), [&](const NFA::Transition &t) {
			std::stringstream ss;
			std::string rel;
			if (t.label.isRelation()) {
				if (theory.hasInfo(*t.label.getRelation())) {
					rel = theory.getName(*t.label.getRelation());
				} else {
					rel = std::to_string(t.label.getRelation()->getID());
				}
				if (t.label.getRelation()->isInverse())
					rel += "^-1";
			}
			ss << "[";
			for (const auto &p : t.label.getPreChecks().preds()) {
				ss << theory.getName(p) << (p.isComplement() ? "^-1" : "") << ";";
			}
			ss << "]";
			const auto pred = std::move(ss.str());

			ss.str("");
			ss << "[";
			for (const auto &p : t.label.getPostChecks().preds()) {
				ss << theory.getName(p) << (p.isComplement() ? "^-1" : "") << ";";
			}
			ss << "]";
			const auto post = std::move(ss.str());

			fout << s->getId() << " ->" << t.dest->getId() << "[label = \"" << pred
			     << ";" << rel << ";" << post << "\"];\n";
		});
	});
	fout << "}\n";
}
