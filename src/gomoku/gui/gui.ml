type button =
  | Button of
      { xc : int;
        yc : int;
        half_width : int;
        half_height : int;
        label : string;
        colour : Graphics.color }

type text = Text of {xc : int; yc : int; label : string; colour : Graphics.color}

let window_size = 800

let window_title = "GOMOKU!!!"

let ratio num denom =
  let rec gcd a b = if a = 0 then b else if a > b then gcd b a else gcd (b mod a) a in
  let n' = num / gcd num denom in
  let d' = denom / gcd num denom in
  n' * window_size / d'

let new_window () =
  Graphics.open_graph @@ " " ^ string_of_int window_size ^ "x" ^ string_of_int window_size ;
  Graphics.set_window_title window_title ;
  Graphics.set_text_size 15

let clear_window colour =
  Graphics.clear_graph () ;
  Graphics.set_color colour ;
  Graphics.fill_rect 0 0 window_size window_size

let draw_text (Text {xc; yc; label; colour}) =
  let text_width, text_height = Graphics.text_size label in
  Graphics.set_color colour ;
  Graphics.moveto (xc - (text_width / 2)) (yc - (text_height / 2)) ;
  Graphics.draw_string label

let draw_texts lst = List.iter draw_text lst

let draw_button (Button {xc; yc; half_width; half_height; label; colour}) =
  Graphics.set_color colour ;
  Graphics.fill_rect (xc - half_width) (yc - half_height) (half_width * 2) (half_height * 2) ;
  draw_text @@ Text {xc; yc; label; colour = Graphics.black}

let draw_buttons lst = List.iter draw_button lst

let mouse_click () =
  let st = Graphics.wait_next_event [Graphics.Button_down] in
  (st.Graphics.mouse_x, st.Graphics.mouse_y)

let rec click button_action =
  let check_clicked (x, y) (Button {xc; yc; half_width; half_height; _}, _) =
    abs (x - xc) <= half_width && abs (y - yc) <= half_height
  in
  let mouse_pos = mouse_click () in
  match List.find_opt (check_clicked mouse_pos) button_action with
  | Some (_, action) -> action ()
  | None -> click button_action
