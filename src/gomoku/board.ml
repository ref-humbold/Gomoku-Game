type player = Human | Comp

type field = Free | Border | Stone of player

type gameboard = Gameboard of {fields: field list list; size: int}

exception Incorrect_gameboard of string

exception Incorrect_player of string

let create_board size =
  let rec create_row row col acc =
    if col = 0
    then acc
    else if row = 1 || row = size || col = 1 || col = size
    then create_row row (col - 1) @@ Border :: acc
    else create_row row (col - 1) @@ Free :: acc in
  let rec create row acc =
    if row = 0
    then acc
    else create (row - 1) @@ (create_row row size []) :: acc in
  Gameboard {fields=create size []; size=size}

let set_move (row, col) player (Gameboard g)=
  let rec set_col n row' =
    match row' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
    | x :: xs ->
      if n = 0
      then (Stone player) :: xs
      else x :: (set_col (n - 1) xs) in
  let rec set_row n fields' =
    match fields' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ row"
    | x :: xs ->
      if n = 0
      then (set_col col x) :: xs
      else x :: (set_row (n - 1) xs) in
  Gameboard {g with fields=set_row row g.fields}

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human

let is_free (row, col) (Gameboard {fields; _}) =
  List.nth (List.nth fields row) col = Free
