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

#ifndef KATER_CONFIG_HPP
#define KATER_CONFIG_HPP

#include <string>

struct Config {

	/* Initializes the configuration based on the CLI arguments ARGC/ARGV */
	void parseOptions(int argc, char **argv);

	bool debug;
	std::string debugOnly;
        int verbose;
	bool generate;
	std::string name;
	std::string dir;
	std::string inputFile;

private:
	/* Resets config to its default values */
	void reset();

	/* Prints usage instructions if config parsing fails */
	void printUsage(const char *kater);
};

#endif /* KATER_CONFIG_HPP */
