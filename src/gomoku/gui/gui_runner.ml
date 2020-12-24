let rec run_menu () =
  Menu_gui.display () ;
  Menu_gui.click [run_board; run_stat; exit]

and run_stat () =
  Stat_gui.display () ;
  Stat_gui.click [run_stat; run_menu] ;
  run_menu ()

and run_game size = Game.run size ; run_menu ()

and run_board () =
  Board_gui.display () ;
  run_game @@ Board_gui.choose_size ()

and exit () = ()
