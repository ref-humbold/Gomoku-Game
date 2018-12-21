BUILD = _build/install/default/bin
BIN = bin
EXEC = gomoku

.PHONY : all clean refresh

all : gomoku

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

gomoku :
	dune build
	@mkdir -p $(BIN)
	@ln -sfn ../$(BUILD)/$(EXEC) $(BIN)/$(EXEC)
