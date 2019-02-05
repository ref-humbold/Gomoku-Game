type player = Human | Comp

type field = Free | Border | Stone of player

type gameboard = Gameboard of {fields: field list list; size: int}

exception Incorrect_gameboard of string

exception Incorrect_player of string

let create_board size =
  let rec create_row n m acc =
    if m = 0
    then acc
    else if n = 1 || n = size || m = 1 || m = size
    then create_row n (m - 1) @@ Border :: acc
    else create_row n (m - 1) @@ Free :: acc in
  let rec create n acc =
    if n = 0
    then acc
    else create (n - 1) @@ (create_row n size []) :: acc in
  Gameboard {fields=create size []; size=size}

let set_move (x, y) player (Gameboard g)=
  let rec set_col n row' =
    match row' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
    | col :: cols ->
      if n = 0
      then (Stone player) :: cols
      else col :: (set_col (n - 1) cols) in
  let rec set_row n fields' =
    match fields' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ row"
    | row :: rows ->
      if n = 0
      then (set_col y row) :: rows
      else row :: (set_row (n - 1) rows) in
  Gameboard {g with fields=set_row x g.fields}

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human

let is_free (row, col) (Gameboard {fields; _}) = List.nth (List.nth fields row) col = Free

let get_row n (Gameboard {fields; _}) = List.nth fields n

let get_column m (Gameboard {fields; _}) = List.map (fun lst -> List.nth lst m) fields

let get_sum_diag sum (Gameboard {fields; size}) =
  let rec extract i fields' acc =
    match fields' with
    | [] -> List.rev acc
    | row :: rows ->
      if sum - i < 0 || sum - i > size + 1
      then extract (i + 1) rows acc
      else extract (i + 1) rows @@ (List.nth row @@ sum - i) :: acc in
  extract 0 fields []

let get_diff_diag diff (Gameboard {fields; size}) =
  let rec extract i fields' acc =
    match fields' with
    | [] -> List.rev acc
    | row :: rows ->
      if i - diff < 0 || i - diff > size + 1
      then extract (i + 1) rows acc
      else extract (i + 1) rows @@ (List.nth row @@ i - diff) :: acc in
  extract 0 fields []
