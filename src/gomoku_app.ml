let main () =
  GomokuLib.Stat.prepare_data () ;
  GomokuLib.Gui.new_window () ;
  GomokuLib.Gui_runner.run_menu () ;
  Graphics.close_graph ()

let _ = main ()
