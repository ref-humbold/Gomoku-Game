BUILD_SRC = _build/default/src
BUILD_EXEC = gomoku_app.exe

SRC = src
TEST = test

OUTPUT = buildOut
BIN = $(OUTPUT)/bin

EXECUTABLE = gomoku

.PHONY : all build clean compile format refresh refresh-all

all : build

clean :
	rm -fr $(OUTPUT)
	dune clean

format :
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;

compile :
	mkdir -p $(BIN) $(DIST)
	dune build
	@echo
	cp $(BUILD_SRC)/$(BUILD_EXEC) $(BIN)/$(EXECUTABLE)

build : format compile

refresh : clean build

refresh-all : clean all
