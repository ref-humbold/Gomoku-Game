OCB = ocamlbuild
OCBFLAGS = -use-ocamlfind -I src
BUILD = $(OCB) $(OCBFLAGS)

.PHONY: all clean refresh

all : gomoku.native

clean :
	$(OCB) -clean

refresh : clean all

gomoku.native :
	$(BUILD) gomoku.native

gomoku.byte :
	$(BUILD) gomoku.byte
