open Board

type move =
  | Comp_five of place
  | Human_five of place
  | Comp_mult_three of place
  | Human_mult_three of place
  | Comp_four of place
  | Human_four of place
  | Heuristic

type direction =
  | Row of int * field list
  | Column of int * field list
  | Sum of int * field list
  | Diff of int * field list

type win_line = Five of place * direction | Four of place * direction

type move_info = {mutable queue : move list; mutable last : place}

let moves = {queue = [Heuristic]; last = GP (0, 0)}

let clear_moves () =
  moves.queue <- [Heuristic] ;
  moves.last <- GP (0, 0)

let get_row_dim rn gameboard = Row (rn, get_row rn gameboard)

let get_column_dim cn gameboard = Column (cn, get_column cn gameboard)

let get_sum_diag_dim sum gameboard = Sum (sum, get_sum_diag sum gameboard)

let get_diff_diag_dim diff gameboard = Diff (diff, get_diff_diag diff gameboard)

let get_dirs_at_pos (GP (rn, cn)) gameboard =
  [ get_row_dim rn gameboard; get_column_dim cn gameboard; get_sum_diag_dim (rn + cn) gameboard;
    get_diff_diag_dim (rn - cn) gameboard ]

let get_all_lines gameboard =
  let get_rows {fields; _} = fields in
  let get_columns gameboard' = List.mapi (fun i _ -> get_column i gameboard') gameboard'.fields in
  let get_sum_diags gameboard' =
    let rec get_s sum acc =
      if sum <= gameboard'.size * 2
      then get_s (sum + 1) (get_sum_diag sum gameboard' :: acc)
      else acc
    in
    get_s 2 []
  in
  let get_diff_diags gameboard' =
    let rec get_d diff acc =
      if diff <= gameboard'.size - 1
      then get_d (diff + 1) (get_diff_diag diff gameboard' :: acc)
      else acc
    in
    get_d (-gameboard'.size + 1) []
  in
  List.concat
    [get_rows gameboard; get_columns gameboard; get_sum_diags gameboard; get_diff_diags gameboard]

(* Analyze current board situation *)

let analyze_position player pos gameboard =
  let extract_pos dir i =
    match dir with
    | Row (rn, _) -> ()
    | Column (cn, _) -> ()
    | Sum (sum, _) -> ()
    | Diff (diff, _) -> ()
  in
  let rec check dir i lst acc =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      ()
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      ()
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      ()
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      ()
    | Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Free :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      ()
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 -> ()
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 -> ()
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 -> ()
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: ps when player = p1 && p1 = p2 && p2 = p3 -> ()
    | _ :: ps -> ()
    | [] -> acc
  in
  let check_direction dir =
    match dir with
    | Row (rn, row) -> check dir 0 row []
    | Column (cn, col) -> check dir 0 col []
    | Sum (sum, diag) ->
      let i = max 0 @@ (sum - gameboard.size - 1) in
      check dir i diag []
    | Diff (diff, diag) ->
      let i = max 0 diff in
      check dir i diag []
  in
  List.concat @@ List.map check_direction @@ get_dirs_at_pos pos gameboard

let check_lines player gameboard =
  let rec check acc ln =
    match ln with
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p0 && p0 = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      check (5 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: ps
      when player = p0 && p0 = p1 && p1 = p2 && p2 = p3 ->
      check (4 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: ps when player = p0 && p0 = p1 && p1 = p2 ->
      check (3 :: acc) ps
    | Stone p0 :: Stone p1 :: ps when player = p0 && p0 = p1 -> check (2 :: acc) ps
    | Free :: ps | Border :: ps | Stone _ :: ps -> check acc ps
    | [] -> acc
  in
  let rec cnt num lst =
    match lst with
    | x1 :: (x2 :: _ as xt) -> if x1 = x2 then cnt (num + 1) xt else (x1, num) :: cnt 1 xt
    | [x] -> [(x, num)]
    | [] -> []
  in
  let all_lines = List.concat @@ List.map (check []) @@ get_all_lines gameboard in
  let count lst = List.sort compare @@ cnt 1 @@ List.sort compare lst in
  count all_lines

(* Make heuristic move *)

let extract_frees gameboard =
  let neighbours rn cn =
    [ get_field (GP (rn - 1, cn - 1)) gameboard; get_field (GP (rn - 1, cn)) gameboard;
      get_field (GP (rn - 1, cn + 1)) gameboard; get_field (GP (rn, cn - 1)) gameboard;
      get_field (GP (rn - 1, cn + 1)) gameboard; get_field (GP (rn + 1, cn - 1)) gameboard;
      get_field (GP (rn + 1, cn)) gameboard; get_field (GP (rn + 1, cn + 1)) gameboard ]
  in
  let check_free field =
    match field with
    | Free -> true
    | Border | Stone _ -> false
  in
  let rec extract_row rn cn row =
    match row with
    | Stone _ :: fds ->
      if cn >= 1 && cn <= gameboard.size && (List.exists check_free @@ neighbours cn cm)
      then GP (rn, cn) :: extract_row rn (cn + 1) fds
      else extract_row rn (cn + 1) fds
    | Free :: fds | Border :: fds -> extract_row rn (cn + 1) fds
    | [] -> []
  in
  let rec extract rn fields =
    match fields' with
    | row :: rows ->
      if rn >= 1 && rn <= gameboard.size
      then extract_row rn 0 row :: extract (rn + 1) rows
      else extract (n + 1) rows
    | [] -> []
  in
  List.concat @@ extract 0 gameboard.fields
