type grid = GP of int * int

type player = Comp | Human

type field = Free | Border | Stone of player

type gameboard = {fields : field list list; size : int}

let opponent player =
  match player with
  | Human -> Comp
  | Comp -> Human
