open Board_types
open Board

let move_information = Move_info.initial

let move human_move gameboard =
  let analyzed = Comp_analyzer.analyze human_move (Move_info.last move_information) gameboard in
  ()
