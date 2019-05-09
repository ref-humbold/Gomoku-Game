type player = Human | Comp
type field = Free | Border | Stone of player
type gameboard = {fields: field list list; size: int}

exception Incorrect_gameboard of string
exception Incorrect_player of string

let create_board size =
  let rec create_row n m acc =
    if m = size + 2
    then acc
    else if n = 0 || n = size + 1 || m = 0 || m = size + 1
    then create_row n (m + 1) (Border :: acc)
    else create_row n (m + 1) (Free :: acc)
  in
  let rec create n acc = if n = size + 2 then acc else create (n + 1) (create_row n 0 [] :: acc) in
  {fields= create 0 []; size}

let get_field (n, m) {fields; _} = List.nth (List.nth fields n) m

let get_row n {fields; _} = List.nth fields n

let get_column m {fields; _} = List.map (fun lst -> List.nth lst m) fields

let get_sum_diag sum {fields; size} =
  let rec extract i fields' acc =
    match fields' with
    | [] -> List.rev acc
    | row :: rows ->
      if sum - i < 0 || sum - i > size + 1
      then extract (i + 1) rows acc
      else extract (i + 1) rows (List.nth row (sum - i) :: acc)
  in
  extract 0 fields []

let get_diff_diag diff {fields; size} =
  let rec extract i fields' acc =
    match fields' with
    | [] -> List.rev acc
    | row :: rows ->
      if i - diff < 0 || i - diff > size + 1
      then extract (i + 1) rows acc
      else extract (i + 1) rows (List.nth row (i - diff) :: acc)
  in
  extract 0 fields []

let set_move (n, m) player gameboard =
  let rec set_col i row' =
    match row' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
    | col :: cols -> if i = 0 then Stone player :: cols else col :: set_col (i - 1) cols
  in
  let rec set_row i fields' =
    match fields' with
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ row"
    | row :: rows -> if i = 0 then set_col m row :: rows else row :: set_row (i - 1) rows
  in
  {gameboard with fields= set_row n gameboard.fields}

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human
