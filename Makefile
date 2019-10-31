BUILD_BIN = _build/install/default/bin

BIN = bin
SRC = src

BIN_DIST = gomoku

.PHONY : all build clean compile format refresh

all : compile

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

build : format all

compile :
	dune build
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD_BIN)/$(BIN_DIST) $(BIN)/$(BIN_DIST)

format :
	dune build @fmt --auto-promote 2> /dev/null ; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?') ; do ocp-indent -i $$F ; done
