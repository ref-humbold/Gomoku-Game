let main () =
  Stat.prepare_data () ;
  Gui.new_window () ;
  Gui_runner.run_menu () ;
  Graphics.close_graph ()

let _ = main ()
