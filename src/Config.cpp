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

#include "Config.hpp"
#include "Error.hpp"
#include <climits>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include <iostream>
#include <unistd.h>

void Config::reset()
{
	parallel = true;
	debug = false;
	debugOnly = "";
	verbose = 0;
	generate = false;
	name = "";
	dir = "/tmp";
	inputFile = "";
}

void Config::printUsage(const char *kater)
{
	/* Reset defaults before printing */
	reset();

	printf("OVERVIEW: kater -- consistency check routine generator\n"
	       "Usage: %s [options] <input file>\n"
	       "\n"
	       "OPTIONS:\n"
	       "\n"
	       "-h, --help                  Display this help message and exit\n"
	       "--no-par                    Do not check Kater assertions in parallel.\n"
#ifdef ENABLE_KATER_DEBUG
	       "-d, --debug                 Print all debugging information.\n"
	       "                            Default: %d\n"
	       "--debug-only                Print debugging information of the specified type(s).\n"
	       "                            Default: \"%s\"\n"
#endif
	       "-e, --export                Whether code for dynamic checks will be exported.\n"
	       "                            Default: %d\n"
	       "-n, --name                  Name to be used for the resulting files.\n"
	       "                            Default: \"%s\" (prints to stdout)\n"
	       "-p, --prefix                Directory where the resulting files will be stored.\n"
	       "                            Has no effect without -n.\n"
	       "                            Default: \"%s\"\n"
	       "-v[NUM], --verbose[=NUM]    Print verbose execution information. NUM is optional:\n"
	       "                            0 is quiet; 1 prints status; 2 is noisy;\n"
	       "                            3 is noisier.\n"
	       "                            Default: %d\n",
	       kater, static_cast<int>(parallel),
#ifdef ENABLE_KATER_DEBUG
	       debug, debugOnly.c_str(),
#endif
	       static_cast<int>(generate), name.c_str(), dir.c_str(), verbose);
	exit(0);
}

void Config::parseOptions(int argc, char **argv)
{
	/* Reset defaults before parsing the options */
	reset();

	static constexpr int parallelOpt = 4142;
#ifdef ENABLE_KATER_DEBUG
	static constexpr int debugOnlyOpt = 4242;
#endif

	const char *shortopts = "hden:p:v::";
	const struct option longopts[] = {
		{"help", no_argument, nullptr, 'h'},
		{"no-par", no_argument, nullptr, parallelOpt},
#ifdef ENABLE_KATER_DEBUG
		{"debug", no_argument, nullptr, 'd'},
		{"debug-only", required_argument, nullptr, debugOnlyOpt},
#endif
		{"export", no_argument, nullptr, 'e'},
		{"name", required_argument, nullptr, 'n'},
		{"prefix", required_argument, nullptr, 'p'},
		{"verbose", optional_argument, nullptr, 'v'},
		{nullptr, 0, nullptr, 0} /* Terminator */
	};

	int opt;
	int longindex;
	bool error = false;
	while (!error && (opt = getopt_long(argc, argv, shortopts, longopts, &longindex)) != -1) {
		switch (opt) {
		case 'h':
			printUsage(argv[0]);
			break;
		case parallelOpt:
			parallel = false;
			break;
		case 'e':
			generate = true;
			break;
		case 'n':
			name = optarg;
			break;
		case 'p':
			dir = optarg;
			break;
		case 'v':
			verbose = optarg != nullptr ? atoi(optarg) : 1;
			break;
#ifdef ENABLE_KATER_DEBUG
		case 'd':
			debug = true;
			katerDebug = true;
			break;
		case debugOnlyOpt:
			debugOnly = debugOnly.empty() ? optarg : (debugOnly + optarg);
			addDebugType(optarg);
			break;
#endif
		default: /* '?' */
			error = true;
			break;
		}
	}

	/* If there is a remaining argument, set the filename accordingly */
	if (argc > optind) {
		inputFile = *(argv + optind);
	}

	if (error) {
		printUsage(argv[0]);
	}
}
