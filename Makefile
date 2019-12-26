BUILD_SRC = _build/default/src
BUILD_EXEC = main.exe

BIN = bin
SRC = src

EXEC_DIST = gomoku

.PHONY : all build clean compile format refresh test

all : compile test

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

build : format all

compile :
	dune build
	mkdir -p $(BIN)
	cp $(BUILD_SRC)/$(BUILD_EXEC) $(BIN)/$(EXEC_DIST)

test :
	dune runtest

format :
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?') ;\
	  do ocamlformat -i $$F ; ocp-indent -i $$F ; done
	for F in $$(find $(TEST) -regextype egrep -regex '.+\.mli?') ;\
	  do ocamlformat -i $$F ; ocp-indent -i $$F ; done
