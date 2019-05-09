open Board

let rec choose_stone size =
  let n, m = Game_gui.grid_of_point size @@ Gui.mouse_click () in
  if n >= 1 && n <= size && m >= 1 && m <= size then (n, m) else choose_stone size

let rec move gameboard =
  let pos = choose_stone gameboard.size in
  if Board.get_field pos gameboard = Free then pos else move gameboard
