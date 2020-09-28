BUILD_SRC = _build/default/src
BUILD_EXEC = main.exe

BIN = bin
SRC = src
TEST = test

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
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
	find $(TEST) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
