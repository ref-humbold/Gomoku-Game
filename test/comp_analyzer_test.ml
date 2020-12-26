open OUnit2
open GomokuLib
open Board_types
open Comp_analyzer

(* comp_analyzer_Test_list *)

let convert_to_moves_Then_sorted_moves_list =
  "convert_to_moves Then sorted moves list"
  >:: fun _ ->
  (* given *)
  let bindings =
    [ (GP (10, 10), [(5, Comp); (3, Human); (3, Human)]); (GP (5, 3), [(4, Comp); (3, Human)]);
      (GP (9, 12), [(5, Human); (3, Comp); (3, Human)]); (GP (14, 2), [(4, Human)]) ]
  in
  let map = GridMap.of_seq @@ List.to_seq bindings in
  let expected =
    [ Move_info.Five (Comp, GP (10, 10)); Move_info.Five (Human, GP (9, 12));
      Move_info.Four (Comp, GP (5, 3)); Move_info.Four (Human, GP (14, 2));
      Move_info.Double_three (Human, GP (10, 10)) ]
  in
  (* when *)
  let result () = convert_to_moves map in
  (* then *)
  assert_equal expected @@ result ()

let comp_analyzer_Test_list = test_list [convert_to_moves_Then_sorted_moves_list]

(* comp_analyzer_Test *)

let comp_player_Test = "Tests: Computer analyzer" >::: [comp_analyzer_Test_list]

let _ = run_test_tt_main comp_player_Test
