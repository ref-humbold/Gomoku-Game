open Board

type move =
  | Comp_five of grid
  | Human_five of grid
  | Comp_mult_three of grid
  | Human_mult_three of grid
  | Comp_four of grid
  | Human_four of grid
  | Heuristic

type move_info = {mutable queue : move list; mutable last : grid}

let moves = {queue = [Heuristic]; last = GP (0, 0)}

let clear_moves () =
  moves.queue <- [Heuristic] ;
  moves.last <- GP (0, 0)

(* Make heuristic move *)

let extract_frees gameboard =
  let neighbours rn cn =
    [ get_field (GP (rn - 1, cn - 1)) gameboard; get_field (GP (rn - 1, cn)) gameboard;
      get_field (GP (rn - 1, cn + 1)) gameboard; get_field (GP (rn, cn - 1)) gameboard;
      get_field (GP (rn, cn + 1)) gameboard; get_field (GP (rn + 1, cn - 1)) gameboard;
      get_field (GP (rn + 1, cn)) gameboard; get_field (GP (rn + 1, cn + 1)) gameboard ]
  in
  let check_stone field =
    match field with
    | Stone _ -> true
    | Border | Free -> false
  in
  let rec extract_row rn cn row =
    match row with
    | Free :: fds ->
      if cn >= 1 && cn <= gameboard.size && (List.exists check_stone @@ neighbours rn cn)
      then GP (rn, cn) :: extract_row rn (cn + 1) fds
      else extract_row rn (cn + 1) fds
    | Stone _ :: fds | Border :: fds -> extract_row rn (cn + 1) fds
    | [] -> []
  in
  let rec extract rn fields =
    match fields with
    | row :: rows ->
      if rn >= 1 && rn <= gameboard.size
      then extract_row rn 0 row :: extract (rn + 1) rows
      else extract (rn + 1) rows
    | [] -> []
  in
  List.sort_uniq compare @@ List.concat @@ extract 0 gameboard.fields
