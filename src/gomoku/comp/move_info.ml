open Board_types

type move =
  | Five of player * grid
  | Four of player * grid
  | Double_three of player * grid
  | Heuristic

type move_info = {mutable stack : move list; mutable last : grid option}

let initial = {stack = [Heuristic]; last = None}

let last info = info.last

let set_last info g = info.last <- Some g

let put info moves = info.stack <- moves @ info.stack

let top info =
  match info.stack with
  | [] -> None
  | _ -> Some (List.hd info.stack)

let pop info =
  match info.stack with
  | [] -> ()
  | _ -> info.stack <- List.tl info.stack
