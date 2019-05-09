BUILD = _build/install/default/bin
BIN = bin
EXEC = gomoku

.PHONY : all clean refresh fmt

all : fmt gomoku

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

gomoku :
	dune build
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD)/$(EXEC) $(BIN)/$(EXEC)

fmt :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; test $$? -le 1
