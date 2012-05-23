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
ML_FILES = lexer.ml ast.ml types.ml parser.ml ../../main.ml
MLI_FILES = lexer.mli ast.mli parser.mli 
CMO_FILES = $(patsubst %.ml, %.cmo, $(ML_FILES))
CMI_FILES = $(patsubst %.ml, %.cmi, $(ML_FILES))

all: $(EXE_FILE)

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

lexer.ml: lexer.mll
	ocamllex -o $@ $<

parser.ml parser.mli: parser.mly
	ocamlyacc -v $<

$(EXE_FILE): $(CMO_FILES)
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

.PHONY: clean distclean

-include .depend

depend: lexer.ml lexer.mli types.ml ast.ml ast.mli parser.ml parser.mli ../../main.ml
	$(OCAMLDEP)  $^ > .depend

clean:
	$(RM) -f parser.ml parser.mli parser.output lexer.ml $(CMO_FILES) $(CMI_FILES) *~

distclean: clean
	$(RM) -f $(EXE_FILE) .depend
# DO NOT DELETE
