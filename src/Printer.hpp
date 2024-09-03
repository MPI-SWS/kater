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

#ifndef KATER_PRINTER_HPP
#define KATER_PRINTER_HPP

#include <iostream>
#include <string>

class KatModule;
class DFSParameters;
class Config;

class Printer {
protected:
	Printer(const KatModule &module, const Config &conf);

public:
	/** Generates all consistency checking code from the module */
	virtual void output() = 0;

protected:
	[[nodiscard]] auto getModule() const -> const KatModule & { return module_; }
	[[nodiscard]] auto getConf() const -> const Config & { return config_; }
	[[nodiscard]] auto getPrefix() const -> const std::string & { return prefix_; }

private:
	const KatModule &module_;
	const Config &config_;

	/* Prefix for the names to be printed (class name, filenames, etc) */
	std::string prefix_;
};

#endif /* KATER_PRINTER_HPP */
