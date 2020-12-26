open Board_types

type next_move = Five of player * grid | Four of player * grid | Double_three of player * grid

type move_info = {mutable stack : next_move list; mutable last : grid option}

let initial_move = {stack = []; last = None}

let last_move info = info.last

let set_last_move info g = info.last <- Some g

let put_move info moves = info.stack <- moves @ info.stack

let top_move info =
  match info.stack with
  | [] -> None
  | _ -> Some (List.hd info.stack)

let pop_move info =
  match info.stack with
  | [] -> ()
  | _ -> info.stack <- List.tl info.stack
