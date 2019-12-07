open Board

type move =
  | Comp_five of place
  | Human_five of place
  | Comp_mult_three of place
  | Human_mult_three of place
  | Comp_four of place
  | Human_four of place
  | Any

type direction = Row of int | Column of int | Sum of int * int | Diff of int * int

type win_place = Five of place | Four of place

type move_info = {mutable queue : move list; mutable last : place}

let moves = {queue = [Any]; last = GP (0, 0)}
