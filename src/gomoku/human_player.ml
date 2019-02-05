let rec move (Board.Gameboard {size; _} as gameboard) =
  let pos = Game_gui.choose_stone size in
  if Board.is_free pos gameboard
  then pos
  else move gameboard
