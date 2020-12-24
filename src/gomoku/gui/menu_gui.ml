let buttons =
  [ Gui.Button
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 3 4;
        half_width = 200;
        half_height = 50;
        label = "NEW GAME";
        colour = Graphics.magenta };
    Gui.Button
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 2;
        half_width = 200;
        half_height = 50;
        label = "STATISTICS";
        colour = Graphics.magenta };
    Gui.Button
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 4;
        half_width = 200;
        half_height = 50;
        label = "EXIT";
        colour = Graphics.magenta } ]

let texts =
  [ Gui.Text
      {xc = Gui.ratio 1 2; yc = Gui.ratio 7 8; label = Gui.window_title; colour = Graphics.green};
    Gui.Text
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 8;
        label = "(C) 2019 RAFAL KALETA, MIT LICENSE";
        colour = Graphics.black } ]

let display () = Gui.clear_window Graphics.blue ; Gui.draw_buttons buttons ; Gui.draw_texts texts

let click actions =
  let action = List.nth actions @@ Gui.click buttons in
  action ()
