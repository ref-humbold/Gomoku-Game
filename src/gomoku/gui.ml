type button =
  | Btn of {xc: int; yc: int; width: int; height: int; label: string; colour: Graphics.color}

type text = Txt of {xc: int; yc: int; label: string; colour: Graphics.color}

let window_size = 800

let window_title = "GOMOKU!!!"

let ratio num denom =
  let rec gcd a b = if a = 0 then b else if a > b then gcd b a else gcd (b mod a) a in
  let n' = num / gcd num denom in
  let d' = denom / gcd num denom in
  n' * window_size / d'

let center_text (Txt {xc; yc; label; _}) =
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

let draw_text (Txt {label; colour; _} as text) =
  let x, y = center_text text in
  Graphics.set_color colour ;
  Graphics.moveto x y ;
  Graphics.draw_string label

let draw_texts lst = List.iter draw_text lst

let draw_button (Btn {xc; yc; width; height; label; colour}) =
  Graphics.set_color colour ;
  Graphics.fill_rect (xc - (width / 2)) (yc - (height / 2)) width height ;
  draw_text @@ Txt {xc; yc; label; colour= Graphics.black}

let draw_buttons lst = List.iter draw_button lst

let mouse_click () =
  let st = Graphics.wait_next_event [Graphics.Button_down] in
  (st.Graphics.mouse_x, st.Graphics.mouse_y)

let check_button_clicked (x, y) (Btn {xc; yc; width; height; _}) =
  abs (x - xc) <= width && abs (y - yc) <= height
