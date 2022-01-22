open Board_types
open Board

let check_winner gameboard player (GP (rn, cn)) =
  let rec check_five lst =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _
      when p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _
      when p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone op :: _
      when op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Stone op :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _
      when op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Stone op :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _
      when op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Stone e1 :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone e2 :: _
      when p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player && e1 <> p5 && e2 = e1 ->
      true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone op :: _
      when op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _
      when p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _
      when p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player ->
      true
    | Free :: ps | Border :: ps | Stone _ :: ps -> check_five ps
    | [] -> false
  in
  if List.exists check_five
    @@ [ get_row rn gameboard;
         get_column cn gameboard;
         get_sum_diag (rn + cn) gameboard;
         get_diff_diag (rn - cn) gameboard ]
  then Some player
  else None

let start_game size =
  Random.self_init () ; Comp_player.clear () ; Game_gui.display size ; create_board size

let end_game (winner, moves) = Stat.update_data winner moves ; Game_gui.return winner

let play_game gameboard =
  let rec turn gameboard' counts last_move player =
    let move_pos =
      match player with
      | Human -> Human_player.move gameboard'
      | Comp -> Comp_player.move last_move gameboard'
    in
    let new_counts =
      match player with
      | Human -> Stat.{counts with human_moves_count = counts.human_moves_count + 1}
      | Comp -> Stat.{counts with comp_moves_count = counts.comp_moves_count + 1}
    in
    let new_gameboard = set_move move_pos player gameboard' in
    Game_gui.draw_stone gameboard.size player move_pos ;
    match check_winner new_gameboard player move_pos with
    | None -> turn new_gameboard new_counts move_pos @@ opponent player
    | Some player -> (player, new_counts)
  in
  turn gameboard Stat.{human_moves_count = 0; comp_moves_count = 0} (GP (0, 0)) Human

let run size =
  let gameboard = start_game size in
  let game_result = play_game gameboard in
  end_game game_result
