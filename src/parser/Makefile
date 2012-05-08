########################################################
 # 
 # Makefile for Llama language
 #
 # This file is part of Llammac project.
 #
########################################################

.PHONY: all

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

RM = /bin/rm

OCAMLC = ocamlc
OCAMLC_FLAGS = -g
OCAMLDEP = ocamldep

EXE_FILE = llama$(EXE)
ML_FILES = Lexer.ml Parser.ml Ast.ml ../../main.ml
MLI_FILES = Lexer.mli Parser.mli
CMO_FILES = $(patsubst %.ml, %.cmo, $(ML_FILES))
CMI_FILES = $(patsubst %.ml, %.cmi, $(ML_FILES))

all: $(EXE_FILE)

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly 
	ocamlyacc -v $<

$(EXE_FILE): $(CMO_FILES)
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

.PHONY: clean distclean

-include .depend

depend: Lexer.ml Lexer.mli Parser.ml Parser.mli ../../main.ml
	$(OCAMLDEP)  $^ > .depend

clean:
	$(RM) -f Parser.ml Parser.mli Parser.output Lexer.ml $(CMO_FILES) $(CMI_FILES) *~

distclean: clean
	$(RM) -f $(EXE_FILE) .depend
