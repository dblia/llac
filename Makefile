.PHONY: clean count_lines

EXE_FILE = main.native

all: $(EXE_FILE)

.PHONY: all clean count_lines

$(EXE_FILE):
	ocamlbuild $(EXE_FILE)

clean:
	ocamlbuild -clean

count_lines:
	wc -l $(SRC_FILES)
