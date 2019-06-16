BUILD_BIN = _build/install/default/bin
GOMOKU = gomoku
BIN = bin
SRC = src

.PHONY : all clean compile format refresh

all : format compile

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

compile :
	dune build
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD_BIN)/$(GOMOKU) $(BIN)/$(GOMOKU)

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
