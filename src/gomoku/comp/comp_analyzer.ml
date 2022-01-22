open Board_types
open Board

type direction =
  | Row of int * field list
  | Column of int * field list
  | Sum of int * field list
  | Diff of int * field list

module GridMap = Map.Make (struct
    type t = grid

    let compare = Stdlib.compare
  end)

let get_row_dir rn gameboard = Row (rn, get_row rn gameboard)

let get_column_dir cn gameboard = Column (cn, get_column cn gameboard)

let get_sum_diag_dir sum gameboard = Sum (sum, get_sum_diag sum gameboard)

let get_diff_diag_dir diff gameboard = Diff (diff, get_diff_diag diff gameboard)

(* Get all directions at specified grid point *)
let get_dirs_at_pos (GP (rn, cn)) gameboard =
  [ get_row_dir rn gameboard;
    get_column_dir cn gameboard;
    get_sum_diag_dir (rn + cn) gameboard;
    get_diff_diag_dir (rn - cn) gameboard ]

(* Convert map <grid point, (line length, player)> to sorted list of next moves. *)
let convert_to_moves map =
  let rec convert pos lst acc =
    match lst with
    | (5, player) :: xs -> convert pos xs (Move_info.Five (player, pos) :: acc)
    | (4, player) :: xs -> convert pos xs (Move_info.Four (player, pos) :: acc)
    | (3, player1) :: (3, player2) :: xs when player1 = player2 ->
      convert pos xs (Move_info.Double_three (player1, pos) :: acc)
    | _ :: xs -> convert pos xs acc
    | [] -> acc
  in
  List.sort compare @@ GridMap.fold convert map []

(* Extract all lines for a player at specified point. *)
let analyze_lines player pos gameboard =
  let get_pos dir i =
    match dir with
    | Row (rn, _) -> GP (rn, i)
    | Column (cn, _) -> GP (i, cn)
    | Sum (sum, _) -> GP (sum, sum - i)
    | Diff (diff, _) -> GP (i, i - diff)
  in
  let rec extract dir i lst acc =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = get_pos dir i in
      extract dir (i + 5) ps ((pos, player, 5) :: acc)
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = get_pos dir (i + 1) in
      extract dir (i + 5) ps ((pos, player, 5) :: acc)
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = get_pos dir (i + 2) in
      extract dir (i + 5) ps ((pos, player, 5) :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = get_pos dir (i + 3) in
      extract dir (i + 5) ps ((pos, player, 5) :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Free :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = get_pos dir (i + 4) in
      extract dir (i + 5) ps ((pos, player, 5) :: acc)
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = get_pos dir i in
      extract dir (i + 4) ps ((pos, player, 4) :: acc)
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = get_pos dir (i + 1) in
      extract dir (i + 4) ps ((pos, player, 4) :: acc)
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = get_pos dir (i + 2) in
      extract dir (i + 4) ps ((pos, player, 4) :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = get_pos dir (i + 3) in
      extract dir (i + 4) ps ((pos, player, 4) :: acc)
    | Free :: Stone p1 :: Stone p2 :: ps when player = p1 && p1 = p2 ->
      let pos = get_pos dir i in
      extract dir (i + 3) ps ((pos, player, 3) :: acc)
    | Stone p1 :: Free :: Stone p2 :: ps when player = p1 && p1 = p2 ->
      let pos = get_pos dir (i + 1) in
      extract dir (i + 3) ps ((pos, player, 3) :: acc)
    | Stone p1 :: Stone p2 :: Free :: ps when player = p1 && p1 = p2 ->
      let pos = get_pos dir (i + 2) in
      extract dir (i + 3) ps ((pos, player, 3) :: acc)
    | Free :: ps | Border :: ps | Stone _ :: ps -> extract dir (i + 1) ps acc
    | [] -> acc
  in
  let extract_winning dir =
    match dir with
    | Row (_, row) -> extract dir 0 row []
    | Column (_, col) -> extract dir 0 col []
    | Sum (sum, diag) ->
      let i = max 0 (sum - gameboard.size - 1) in
      extract dir i diag []
    | Diff (diff, diag) ->
      let i = max 0 diff in
      extract dir i diag []
  in
  let rec group acc lst =
    match lst with
    | (pos, player, n) :: lst' ->
      ( match GridMap.find_opt pos acc with
        | Some nums -> group (GridMap.add pos ((n, player) :: nums) acc) lst'
        | None -> group (GridMap.add pos [(n, player)] acc) lst' )
    | [] -> acc
  in
  group GridMap.empty @@ List.concat @@ List.map extract_winning @@ get_dirs_at_pos pos gameboard

(* Create list of next moves after a round of human and computer moves. *)
let analyze human_move comp_move gameboard =
  let merge_func _ lst1 lst2 =
    match (lst1, lst2) with
    | Some xs, Some ys -> Some (List.sort compare @@ List.rev_append xs ys)
    | Some xs, None -> Some (List.sort compare xs)
    | None, Some ys -> Some (List.sort compare ys)
    | None, None -> None
  in
  let comp_lines =
    match comp_move with
    | Some mv -> analyze_lines Comp mv gameboard
    | None -> GridMap.empty
  and human_lines = analyze_lines Human human_move gameboard in
  convert_to_moves @@ GridMap.merge merge_func comp_lines human_lines
