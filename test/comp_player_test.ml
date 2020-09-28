(* Tests: Computer player AI *)
open OUnit2
open GomokuLib.Board
open GomokuLib.Comp_player
open Utils

(* comp_player test *)

let comp_player_Test = "Tests: Computer player AI" >::: []

let _ = run_test_tt_main comp_player_Test
