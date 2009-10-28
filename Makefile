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
