open Board

type move =
  | Five of player * grid
  | Four of player * grid
  | Double_three of player * grid
  | Heuristic

type t = {mutable stack : move list; mutable last : grid option}

let move_info = {stack = []; last = None}

let init () =
  move_info.stack <- [Heuristic] ;
  move_info.last <- None

let last = move_info.last

let set_last g = move_info.last <- Some g

let put moves = move_info.stack <- moves @ move_info.stack

let top =
  match move_info.stack with
  | [] -> None
  | _ -> Some (List.hd move_info.stack)

let pop () =
  match move_info.stack with
  | [] -> ()
  | _ -> move_info.stack <- List.tl move_info.stack
