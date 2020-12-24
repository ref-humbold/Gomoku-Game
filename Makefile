BUILD_SRC = _build/default/src
BUILD_EXEC = main.exe

BIN = bin
SRC = src
TEST = test

EXECUTABLE = gomoku

.PHONY : all build clean compile dirs format refresh

all : compile

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

dirs :
	mkdir -p $(BIN)

build : format all

compile : dirs
	dune build
	@echo
	cp $(BUILD_SRC)/$(BUILD_EXEC) $(BIN)/$(EXECUTABLE)

format :
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
