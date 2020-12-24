let buttons =
  [ Gui.Button
      { xc = Gui.ratio 1 4;
        yc = Gui.ratio 3 4;
        half_width = 100;
        half_height = 50;
        label = "15 x 15";
        colour = Graphics.green };
    Gui.Button
      { xc = Gui.ratio 3 4;
        yc = Gui.ratio 3 4;
        half_width = 100;
        half_height = 50;
        label = "17 x 17";
        colour = Graphics.green };
    Gui.Button
      { xc = Gui.ratio 1 4;
        yc = Gui.ratio 1 2;
        half_width = 100;
        half_height = 50;
        label = "19 x 19";
        colour = Graphics.green };
    Gui.Button
      { xc = Gui.ratio 3 4;
        yc = Gui.ratio 1 2;
        half_width = 100;
        half_height = 50;
        label = "21 x 21";
        colour = Graphics.green };
    Gui.Button
      { xc = Gui.ratio 1 4;
        yc = Gui.ratio 1 4;
        half_width = 100;
        half_height = 50;
        label = "23 x 23";
        colour = Graphics.green };
    Gui.Button
      { xc = Gui.ratio 3 4;
        yc = Gui.ratio 1 4;
        half_width = 100;
        half_height = 50;
        label = "25 x 25";
        colour = Graphics.green } ]

let text =
  Gui.Text
    {xc = Gui.ratio 1 2; yc = Gui.ratio 9 10; label = "CHOOSE BOARD SIZE"; colour = Graphics.black}

let display () = Gui.clear_window Graphics.cyan ; Gui.draw_text text ; Gui.draw_buttons buttons

let choose_size () =
  let add_action i button = (button, fun () -> 15 + (i * 2)) in
  Gui.click @@ List.mapi add_action buttons
