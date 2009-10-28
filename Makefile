CAML = ocamlopt.opt

PROGRAMS = lextest parsetest

all: $(PROGRAMS)

%.cmi: %.mli
	$(CAML) -c $<

%.cmx: %.ml
	$(CAML) -c $<

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.cmx: parser.cmi
reserved.cmi: parser.cmi
reserved.cmx: parser.cmi reserved.cmi
lexer.cmx: parser.cmi reserved.cmi

lextest.cmx: parser.cmi lexer.cmx
parsetest.cmx: parser.cmi lexer.cmx

lextest: parser.cmx reserved.cmx lexer.cmx lextest.cmx
parsetest: parser.cmx reserved.cmx lexer.cmx  parsetest.cmx

$(PROGRAMS): %:
	$(CAML) -o $@ $^

.PHONY: clean

clean:
	rm -f $(PROGRAMS) parser.{mli,ml} lexer.ml *.cm[iox] *.o *~ .*~ \#*\#
