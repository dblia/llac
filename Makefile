########################################################
 # 
 # Makefile for Llama language
 #
 # This file is part of Llammac project.
 #
########################################################

.PHONY: all clean distclean

# OS type: Linux/Win DJGPP

ifdef OS
   EXE=.exe
else
   EXE=
endif

RM = /bin/rm

OCAMLC = ocamlc
OCAMLC_FLAGS = -g

EXE_FILES = llama$(EXE)
ML_FILES = Lexer.ml
MLI_FILES = Lexer.mli
CMO_FILES = $(patsubst %.ml, %.cmo, $(ML_FILES))
CMI_FILES = $(patsubst %.ml, %.cmi, $(ML_FILES))

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

llama$(EXE): Lexer.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: src/parser/Lexer.mll
	ocamllex -o $@ $<

clean:
	$(RM) -f $(ML_FILES) $(CMO_FILES) $(CMI_FILES)

distclean: clean
	$(RM) -f $(EXE_FILES)
