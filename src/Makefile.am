.PHONY: lint

AM_YFLAGS = -d -Wno-yacc
AM_CXXFLAGS = -std=c++17

if kater
  BUILT_SOURCES = Parser.hpp
  bin_PROGRAMS = kater
  parsing_files = Lexer.lpp Parser.ypp
  cpp_files = \
	main.cpp \
	Config.cpp \
	Constraint.cpp \
	Counterexample.cpp \
	Error.cpp \
	Kater.cpp \
	KatModule.cpp \
	NFA.cpp	\
	ParsingDriver.cpp \
	Predicate.cpp \
	Printer.cpp \
	Relation.cpp \
	RegExp.cpp \
	Saturation.cpp \
	TransLabel.cpp \
	Utils.cpp
  kater_SOURCES = $(parsing_files) $(cpp_files)
  TESTS = ../scripts/checkall.sh
endif

lint: $(cpp_files)
	-$(CPPLINT) -header-filter='.*' -config-file='../.clang-tidy' $^

check-local: lint
