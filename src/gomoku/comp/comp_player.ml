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

let move human_move gameboard =
  let analyzed = Comp_analyzer.analyze human_move gameboard in
  ()
