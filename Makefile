CAML = ocamlopt.opt

PROGRAMS = lextest parsetest pptest

all: $(PROGRAMS)

%.cmi: %.mli
	$(CAML) -c $<

%.cmx: %.ml
	$(CAML) -c $<

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

syntax.cmx: syntax.cmi
parser.cmx: syntax.cmi parser.cmi
reserved.cmi: parser.cmi
reserved.cmx: parser.cmi reserved.cmi
lexer.cmx: parser.cmi reserved.cmi
pretty.cmx: syntax.cmi pretty.cmi

lextest.cmx: syntax.cmi parser.cmi lexer.cmx
parsetest.cmx: syntax.cmi parser.cmi lexer.cmx
pptest.cmx: syntax.cmi pretty.cmi parser.cmi lexer.cmx

lextest: syntax.cmx parser.cmx reserved.cmx lexer.cmx lextest.cmx
parsetest: syntax.cmx parser.cmx reserved.cmx lexer.cmx parsetest.cmx
pptest: syntax.cmx pretty.cmx parser.cmx reserved.cmx lexer.cmx pptest.cmx

$(PROGRAMS): %:
	$(CAML) -o $@ $^

.PHONY: clean

clean:
	rm -f $(PROGRAMS) parser.{mli,ml} lexer.ml *.cm[iox] *.o *~ .*~ \#*\#
