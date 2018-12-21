DIR = _build/install/default/bin
EXEC = gomoku

.PHONY : all clean refresh

all : gomoku

clean :
	rm -f gomoku
	dune clean

refresh : clean all

gomoku :
	dune build
	@ln -s $(DIR)/$(EXEC) bin/$(EXEC)
