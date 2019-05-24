BUILD_BIN = _build/install/default/bin
GOMOKU = gomoku
SRC = src

.PHONY : all clean compile format refresh

all : format compile

clean :
	rm -f $(GOMOKU)
	dune clean

refresh : clean all

compile :
	dune build
	ln -sfn $(BUILD_BIN)/$(GOMOKU)

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
