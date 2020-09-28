open GomokuLib

module Printers = struct
  let print_grid_list lst =
    let grid (Board.GP (rn, cn)) = "GP (" ^ string_of_int rn ^ ", " ^ string_of_int cn ^ ")" in
    "[" ^ String.concat "; " (List.map grid lst) ^ "]"
end
