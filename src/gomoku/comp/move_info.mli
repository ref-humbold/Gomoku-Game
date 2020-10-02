open Board

type move =
  | Five of player * grid
  | Four of player * grid
  | Double_three of player * grid
  | Heuristic

val init : unit -> unit

val last : grid option

val set_last : grid -> unit

val put : move list -> unit

val top : move option

val pop : unit -> unit
