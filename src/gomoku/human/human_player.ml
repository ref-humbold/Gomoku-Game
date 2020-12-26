open Board_types

let rec choose_stone size =
  let rn, cn = Game_gui.grid_of_point size @@ Gui.mouse_click () in
  if rn >= 1 && rn <= size && cn >= 1 && cn <= size then GP (rn, cn) else choose_stone size

let rec move gameboard =
  let pos = choose_stone gameboard.size in
  if Board.get_field pos gameboard = Free then pos else move gameboard
