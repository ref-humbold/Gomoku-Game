open Board_types

exception Incorrect_gameboard of string

(* size = 3: Border Border Border Border Border Border Free Free Free Border Border Free Free Free
   Border Border Free Free Free Border Border Border Border Border Border
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

let get_field (GP (rn, cn)) {fields; _} = List.nth (List.nth fields rn) cn

let get_row rn {fields; _} = List.nth fields rn

let get_column cn {fields; _} = List.map (fun lst -> List.nth lst cn) fields

let get_sum_diag sum {fields; size} =
  let rec extract rn fields' acc =
    match fields' with
    | row :: rows ->
      let cn = sum - rn in
      if cn < 0 || cn > size + 1
      then extract (rn + 1) rows acc
      else extract (rn + 1) rows (List.nth row cn :: acc)
    | [] -> List.rev acc
  in
  extract 0 fields []

let get_diff_diag diff {fields; size} =
  let rec extract rn fields' acc =
    match fields' with
    | row :: rows ->
      let cn = rn - diff in
      if cn < 0 || cn > size + 1
      then extract (rn + 1) rows acc
      else extract (rn + 1) rows (List.nth row cn :: acc)
    | [] -> List.rev acc
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
