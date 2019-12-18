open Board

type move =
  | Comp_five of place
  | Human_five of place
  | Comp_mult_three of place
  | Human_mult_three of place
  | Comp_four of place
  | Human_four of place
  | Heuristic

type move_info = {mutable queue : move list; mutable last : place}

let moves = {queue = [Heuristic]; last = GP (0, 0)}

let clear_moves () =
  moves.queue <- [Heuristic] ;
  moves.last <- GP (0, 0)

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
