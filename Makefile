BUILD_SRC = _build/default/src
BUILD_EXEC = main.exe

BIN = bin
SRC = src

SOURCES = $(wildcard $(SRC)/*.{ml,mli})
TEST_FILES = $(wildcard $(TEST)/*.{ml,mli})
EXECUTABLE = gomoku

.PHONY : all build clean compile dirs format refresh test

all : compile test

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

dirs :
	mkdir -p $(BIN)

build : format all

compile : dirs
	dune build
	cp $(BUILD_SRC)/$(BUILD_EXEC) $(BIN)/$(EXECUTABLE)

test :
	dune runtest

format :
	for F in $(SOURCES) ; do ocamlformat -i $$F ; ocp-indent -i $$F ; done
	for F in $(TEST_FILES) ; do ocamlformat -i $$F ; ocp-indent -i $$F ; done
