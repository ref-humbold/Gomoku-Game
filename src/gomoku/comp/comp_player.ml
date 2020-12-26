let move_memory = Move_info.initial_move

let move human_move gameboard =
  let analyzed = Comp_analyzer.analyze human_move (Move_info.last_move move_memory) gameboard in
  ()
