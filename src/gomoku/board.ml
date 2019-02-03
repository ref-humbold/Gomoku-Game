type player = Human | Comp

type field = Free | Border | Stone of player

(* type gameboard = field list list *)
type gameboard = player option option list list

exception Incorrect_gameboard of string

exception Incorrect_player of string

let create_board size =
  let rec create_row row col acc =
    if col = 0
    then acc
    else if row = 1 || row = size || col = 1 || col = size
    (* then create_row row (col - 1) @@ Border :: acc
       else create_row row (col - 1) @@ Empty :: acc in *)
    then create_row row (col - 1) @@ (Some None) :: acc
    else create_row row (col - 1) @@ None :: acc in
  let rec create_board row acc =
    if row = 0
    then acc
    else create_board (row - 1) @@ (create_row row size []) :: acc in
  create_board size []

let set_move (row, col) player game =
  let rec set_col n row' =
    match row' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
    | x :: xs ->
      if n = 0
      (* then (Stone player) :: xs *)
      then (Some (Some player)) :: xs
      else x :: (set_col (n - 1) xs) in
  let rec set_row n gameboard' =
    match gameboard' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ row"
    | x :: xs ->
      if n = 0
      then (set_col col x) :: xs
      else x :: (set_row (n - 1) xs) in
  set_row row game

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human

let is_free (row, col) gameboard =
  match List.nth (List.nth gameboard row) col with
  | None -> true
  | Some _ -> false

(* let is_free (row, col) gameboard =
   List.nth (List.nth gameboard row) col = Free *)
