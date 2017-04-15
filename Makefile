CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLFLAGS = -w A

GRAPHICS = graphics.cmxa -cclib -lgraphics -cclib -L/usr/X11R6/lib -cclib -lX11

all : gomoku

clean :
	rm -f *.cmi *.cmx *.cmo *.o

refresh : clean all

gomoku : gui.cmx board.cmx stat.cmx menu_gui.cmx board_gui.cmx stat_gui.cmx game_gui.cmx comp_player.cmx human_player.cmx game.cmx gui_runner.cmx gomoku.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) gui.cmx board.cmx stat.cmx menu_gui.cmx board_gui.cmx stat_gui.cmx game_gui.cmx comp_player.cmx human_player.cmx game.cmx gui_runner.cmx gomoku.cmx -o gomoku

gomoku.cmx : gui.cmx board.cmx stat.cmx gui_runner.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c gomoku.ml -o gomoku.cmx

gui_runner.cmx : gui_runner.ml gui.cmx menu_gui.cmx stat_gui.cmx board_gui.cmx game.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c gui_runner.ml -o gui_runner.cmx

game.cmx : game.ml board.cmx game_gui.cmx human_player.cmx comp_player.cmx
	$(CAMLOPT) $(CAMLFLAGS) -c game.ml -o game.cmx

human_player.cmx : human_player.ml board.cmx game_gui.cmx
	$(CAMLOPT) $(CAMLFLAGS) -c human_player.ml -o human_player.cmx

comp_player.cmx : comp_player.ml board.cmx
	$(CAMLOPT) $(CAMLFLAGS) -c comp_player.ml -o comp_player.cmx

game_gui.cmx : game_gui.ml board.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c game_gui.ml -o game_gui.cmx

stat_gui.cmx : stat_gui.ml stat.cmx gui.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c stat_gui.ml -o stat_gui.cmx

board_gui.cmx : board_gui.ml gui.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c board_gui.ml -o board_gui.cmx

menu_gui.cmx : menu_gui.ml gui.cmx
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c menu_gui.ml -o menu_gui.cmx

stat.cmx : stat.ml board.cmx
	$(CAMLOPT) $(CAMLFLAGS) -c stat.ml -o stat.cmx

board.cmx : board.ml
	$(CAMLOPT) $(CAMLFLAGS) -c board.ml -o board.cmx

gui.cmx : gui.ml
	$(CAMLOPT) $(CAMLFLAGS) $(GRAPHICS) -c gui.ml -o gui.cmx