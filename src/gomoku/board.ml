type player = Human | Comp

type gameboard = player option option list list

exception Incorrect_gameboard of string

exception Incorrect_player of string

let create_board size =
  let rec create_row rownum i acc =
    if i = 0
    then acc
    else if rownum = 1 || rownum = size || i = 1 || i = size
    then create_row rownum (i - 1) @@ (Some None) :: acc
    else create_row rownum (i - 1) @@ None :: acc in
  let rec create rownum acc =
    if rownum = 0
    then acc
    else create (rownum - 1) @@ (create_row rownum size []) :: acc in
  create size []

let set_move (row, col) player game =
  let rec set_col n row' =
    match row' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
    | x :: xs ->
      if n = 0
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
