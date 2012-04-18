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
OCMALDEP=ocamldep

EXE_FILES = llama$(EXE)
ML_FILES = Lexer.ml Parser.ml main.ml
MLI_FILES = Lexer.mli Parser.mli
CMO_FILES = $(patsubst %.ml, %.cmo, $(ML_FILES))
CMI_FILES = $(patsubst %.ml, %.cmi, $(ML_FILES))

all: llama$(EXE)

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

Lexer.ml: src/parser/Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: src/parser/Parser.mly
	ocamlyacc -v $<

llama$(EXE): Lexer.cmo Parser.cmo main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

.PHONY: clean distclean

-include .depend

depend: $(ML_FILES) $(MLI_FILES)
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) -f Lexer.ml Parser.ml Parser.output $(CMO_FILES) $(CMI_FILES) *~

distclean: clean
	$(RM) -f $(EXE_FILES) .depend
