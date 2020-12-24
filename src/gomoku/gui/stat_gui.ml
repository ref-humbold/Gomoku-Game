let buttons =
  ( Gui.Button
      { xc = Gui.ratio 3 4;
        yc = Gui.ratio 2 16;
        width = 160;
        height = 50;
        label = "BACK";
        colour = Graphics.red },
    Gui.Button
      { xc = Gui.ratio 1 4;
        yc = Gui.ratio 2 16;
        width = 160;
        height = 50;
        label = "CLEAR";
        colour = Graphics.blue } )

let rec check_click () =
  let mouse_pos = Gui.mouse_click () in
  if Gui.check_button_clicked mouse_pos @@ fst buttons
  then 0
  else if Gui.check_button_clicked mouse_pos @@ snd buttons
  then (Stat.clear () ; 1)
  else check_click ()

let display () =
  let Stat.{human_moves; comp_moves; won; lost; sum_human_moves; sum_comp_moves; opened} =
    Stat.read ()
  in
  let show_info xc yc label = Gui.draw_text @@ Gui.Text {xc; yc; label; colour = Graphics.black} in
  let show_number desc number = desc ^ string_of_int number in
  Gui.clear_window Graphics.yellow ;
  Graphics.set_color Graphics.black ;
  show_info (Gui.ratio 1 2) (Gui.ratio 14 16) "STATISTICS:" ;
  show_info (Gui.ratio 1 2) (Gui.ratio 12 16) (show_number "NUMBER OF PROGRAMME RUNS: " opened) ;
  show_info (Gui.ratio 1 2) (Gui.ratio 11 16) (show_number "NUMBER OF PLAYED GAMES: " (won + lost)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 10 16) (show_number "WON: " won) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 10 16) (show_number "LOST: " lost) ;
  show_info
    (Gui.ratio 1 2)
    (Gui.ratio 9 16)
    (show_number "TOTAL NUMBER OF MOVES: " (sum_human_moves + sum_comp_moves)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 8 16) (show_number "YOURS: " sum_human_moves) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 8 16) (show_number "COMPUTER'S: " sum_comp_moves) ;
  show_info
    (Gui.ratio 1 2)
    (Gui.ratio 6 16)
    (show_number "NUMBER OF MOVES IN LAST GAME: " (human_moves + comp_moves)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 5 16) (show_number "YOURS: " human_moves) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 5 16) (show_number "COMPUTER'S: " comp_moves) ;
  Gui.draw_buttons [fst buttons; snd buttons]
