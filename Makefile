BUILD_BIN = _build/install/default/bin

SRC = src
EXEC = gomoku

.PHONY : all clean refresh format

all : gomoku

clean :
	rm -f $(EXEC)
	dune clean

refresh : clean all

gomoku : format
	dune build
	ln -sfn $(BUILD_BIN)/$(EXEC)

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; test $$? -le 1
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
