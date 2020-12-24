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

let center_text (Text {xc; yc; label; _}) =
  let xt, yt = Graphics.text_size label in
  (xc - (xt / 2), yc - (yt / 2))

let new_window () =
  Graphics.open_graph @@ " " ^ string_of_int window_size ^ "x" ^ string_of_int window_size ;
  Graphics.set_window_title window_title ;
  Graphics.set_text_size 15

let clear_window colour =
  Graphics.clear_graph () ;
  Graphics.set_color colour ;
  Graphics.fill_rect 0 0 window_size window_size

let draw_text (Text {label; colour; _} as text) =
  let x, y = center_text text in
  Graphics.set_color colour ; Graphics.moveto x y ; Graphics.draw_string label

let draw_texts lst = List.iter draw_text lst

let draw_button (Button {xc; yc; half_width; half_height; label; colour}) =
  Graphics.set_color colour ;
  Graphics.fill_rect (xc - half_width) (yc - half_height) (half_width * 2) (half_height * 2) ;
  draw_text @@ Text {xc; yc; label; colour = Graphics.black}

let draw_buttons lst = List.iter draw_button lst

let mouse_click () =
  let st = Graphics.wait_next_event [Graphics.Button_down] in
  (st.Graphics.mouse_x, st.Graphics.mouse_y)

let rec click buttons =
  let check_clicked (x, y) i (Button {xc; yc; half_width; half_height; _}) =
    if abs (x - xc) <= half_width && abs (y - yc) <= half_height then i else -1
  in
  let mouse_pos = mouse_click () in
  let clicked = List.mapi (check_clicked mouse_pos) buttons in
  match List.find_opt (fun i -> i >= 0) clicked with
  | Some i -> i
  | None -> click buttons
