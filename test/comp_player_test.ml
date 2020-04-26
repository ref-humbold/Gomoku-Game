(* Tests: Computer player AI *)
open OUnit2
open GomokuLib.Board
open GomokuLib.Comp_player
open Utils

(* extract_frees *)

let extract_frees_When_empty_gameboard_Then_empty_list =
  "Extract free neighbouring positions from empty gameboard"
  >:: fun _ ->
    let gameboard = create_board 5 in
    let result = extract_frees gameboard in
    assert_equal ~printer:Printers.print_grid_list [] result

let extract_frees_When_not_empty_gameboard_Then_sorted_unique_places =
  "Extract free neighbouring positions from non-empty gameboard"
  >:: fun _ ->
    let gameboard = set_move (GP (3, 3)) Comp @@ set_move (GP (3, 2)) Human @@ create_board 5 in
    let result = extract_frees gameboard in
    assert_equal
      ~printer:Printers.print_grid_list
      [ GP (2, 1); GP (2, 2); GP (2, 3); GP (2, 4); GP (3, 1); GP (3, 4); GP (4, 1); GP (4, 2);
        GP (4, 3); GP (4, 4) ]
      result

let extract_frees_Test =
  test_list
    [ extract_frees_When_empty_gameboard_Then_empty_list;
      extract_frees_When_not_empty_gameboard_Then_sorted_unique_places ]

(* comp_player test *)

let comp_player_Test = "Tests: Computer player AI" >::: [extract_frees_Test]

let _ = run_test_tt_main comp_player_Test
