# Joust: a Java lexer, parser, and pretty-printer written in OCaml
# Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
# Released under the GNU General Public License

VERSION = 0.5
PACKAGE = joust-$(VERSION)

CAML = ocamlopt.opt

PROGRAMS = lextest parsetest pptest comtest

all: $(PROGRAMS)

%.cmi: %.mli
	$(CAML) -c $<

%.cmx: %.ml
	$(CAML) -c $<

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

-include .depend

MODULES = source.cmx syntax.cmx parser.cmx reserved.cmx lexer.cmx 

lextest: $(MODULES) lextest.cmx
parsetest: $(MODULES) parsetest.cmx
pptest: $(MODULES) pretty.cmx pptest.cmx
comtest: $(MODULES) pretty.cmx comtest.cmx

$(PROGRAMS): %:
	$(CAML) -o $@ $^

.PHONY: clean depend

clean:
	-rm -f $(PROGRAMS) parser.{mli,ml} lexer.ml *.cm[iox] *.o *~ .*~ \#*\#

depend: parser.mli parser.ml lexer.ml
	ocamldep *.{mli,ml} > .depend

tarball:
	@ln -s . $(PACKAGE); \
	tar cvzf $(PACKAGE).tar.gz $(PACKAGE)/{copyright,README,Makefile,lexer.mll,parser.mly,*.mli,*.ml,*.sh}; \
	rm $(PACKAGE)
