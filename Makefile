# Joust: a Java lexer, parser, and pretty-printer written in OCaml
# Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

VERSION = 0.8

MODULES = source syntax parser reserved lexer pretty
PROGRAMS = lextest parsetest pptest comtest

include ~/proj/ocaml/Makefile
