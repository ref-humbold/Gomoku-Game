let buttons =
  ( Gui.Btn
      { xc = Gui.ratio 3 4;
        yc = Gui.ratio 2 16;
        width = 160;
        height = 50;
        label = "BACK";
        colour = Graphics.red },
    Gui.Btn
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
  let show_info xc yc text =
    Gui.draw_text @@ Gui.Txt {xc; yc; label = text; colour = Graphics.black}
  in
  Gui.clear_window Graphics.yellow ;
  Graphics.set_color Graphics.black ;
  show_info (Gui.ratio 1 2) (Gui.ratio 14 16) "STATISTICS:" ;
  show_info (Gui.ratio 1 2) (Gui.ratio 12 16) ("NUMBER OF PROGRAMME STARTS: " ^ string_of_int opened) ;
  show_info
    (Gui.ratio 1 2)
    (Gui.ratio 11 16)
    ("NUMBER OF PLAYED GAMES: " ^ string_of_int (won + lost)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 10 16) ("WON: " ^ string_of_int won) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 10 16) ("LOST: " ^ string_of_int lost) ;
  show_info
    (Gui.ratio 1 2)
    (Gui.ratio 9 16)
    ("TOTAL NUMBER OF MOVES: " ^ string_of_int (sum_human_moves + sum_comp_moves)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 8 16) ("YOURS: " ^ string_of_int sum_human_moves) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 8 16) ("COMPUTER'S: " ^ string_of_int sum_comp_moves) ;
  show_info
    (Gui.ratio 1 2)
    (Gui.ratio 6 16)
    ("NUMBER OF MOVES IN LAST GAME: " ^ string_of_int (human_moves + comp_moves)) ;
  show_info (Gui.ratio 1 4) (Gui.ratio 5 16) ("YOURS: " ^ string_of_int human_moves) ;
  show_info (Gui.ratio 3 4) (Gui.ratio 5 16) ("COMPUTER'S: " ^ string_of_int comp_moves) ;
  Gui.draw_button @@ fst buttons ;
  Gui.draw_button @@ snd buttons
