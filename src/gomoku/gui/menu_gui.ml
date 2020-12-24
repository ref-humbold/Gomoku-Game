let buttons =
  [ Gui.Btn
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 3 4;
        width = 400;
        height = 100;
        label = "NEW GAME";
        colour = Graphics.magenta };
    Gui.Btn
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 2;
        width = 400;
        height = 100;
        label = "STATISTICS";
        colour = Graphics.magenta };
    Gui.Btn
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 4;
        width = 400;
        height = 100;
        label = "EXIT";
        colour = Graphics.magenta } ]

let texts =
  [ Gui.Txt
      {xc = Gui.ratio 1 2; yc = Gui.ratio 7 8; label = Gui.window_title; colour = Graphics.green};
    Gui.Txt
      { xc = Gui.ratio 1 2;
        yc = Gui.ratio 1 8;
        label = "(C) 2019 RAFAL KALETA, MIT LICENSE";
        colour = Graphics.black } ]

let display () = Gui.clear_window Graphics.blue ; Gui.draw_buttons buttons ; Gui.draw_texts texts

let rec click_button () =
  let mouse_pos = Gui.mouse_click () in
  let clicked = List.map (Gui.check_button_clicked mouse_pos) buttons in
  let rec choose_action lst i =
    match lst with
    | true :: _ -> Some i
    | false :: xs -> choose_action xs (i + 1)
    | [] -> None
  in
  match choose_action clicked 0 with
  | Some index -> index
  | None -> click_button ()