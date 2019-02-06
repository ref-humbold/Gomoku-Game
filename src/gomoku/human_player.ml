let rec move (Board.Gameboard {size; _} as gameboard) =
  let pos = Game_gui.choose_stone size in
  if Board.get_field pos gameboard = Board.Free
  then pos
  else move gameboard
