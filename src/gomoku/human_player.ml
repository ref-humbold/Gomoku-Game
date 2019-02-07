let rec choose_stone size =
  let n, m = Game_gui.grid_of_point size @@ Gui.mouse_click () in
  if n >= 1 && n <= size && m >= 1 && m <= size
  then (n, m)
  else choose_stone size

let rec move (Board.Gameboard {size; _} as gameboard) =
  let pos = choose_stone size in
  if Board.get_field pos gameboard = Board.Free
  then pos
  else move gameboard
