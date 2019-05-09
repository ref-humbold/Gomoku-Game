let rec run_menu () =
  Menu_gui.display () ;
  List.nth [run_board; run_stat; exit] (Menu_gui.click_button ()) ()

and run_stat () =
  Stat_gui.display () ;
  List.nth [run_menu; run_stat] (Stat_gui.check_click ()) () ;
  run_menu ()

and run_game size =
  Game.run size ;
  run_menu ()

and run_board () =
  Board_gui.display () ;
  run_game @@ Board_gui.choose_size ()

and exit () = ()
