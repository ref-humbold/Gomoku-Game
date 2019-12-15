type grid = GP of int * int

type player = Human | Comp

type field = Free | Border | Stone of player

type gameboard = {fields : field list list; size : int}

exception Incorrect_gameboard of string

exception Incorrect_player of string

(*
   Border  Border  Border  Border
   Border  Free    Free    Border
   Border  Free    Free    Border
   Border  Border  Border  Border
*)
let create_board size =
  let rec create_row rn cn acc =
    if cn = size + 2
    then acc
    else if rn = 0 || rn = size + 1 || cn = 0 || cn = size + 1
    then create_row rn (cn + 1) (Border :: acc)
    else create_row rn (cn + 1) (Free :: acc)
  in
  let rec create rn acc =
    if rn = size + 2 then acc else create (rn + 1) (create_row rn 0 [] :: acc)
  in
  {fields = create 0 []; size}

let get_field (GP (rn, cn)) {fields; _} = List.nth (List.nth fields rn) rm

let get_row rn {fields; _} = List.nth fields rn

let get_column cn {fields; _} = List.map (fun lst -> List.nth lst cn) fields

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

let set_move (GP (rn, cn)) player gameboard =
  let rec set_col j row' =
    match row' with
    | col :: cols -> if j = 0 then Stone player :: cols else col :: set_col (j - 1) cols
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ column"
  in
  let rec set_row i fields' =
    match fields' with
    | row :: rows -> if i = 0 then set_col cn row :: rows else row :: set_row (i - 1) rows
    | [] -> raise @@ Incorrect_gameboard "Board.set_move @ row"
  in
  {gameboard with fields = set_row rn gameboard.fields}

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human
