open Board

let check_winner gameboard player (x, y) =
  let rec check lst =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _ when
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _ when
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone op :: _ when
        op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Stone op :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _ when
        op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Stone op :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _ when
        op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Stone e1 :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone e2 :: _ when
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player && e1 <> p5 && e2 = e1 -> true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Stone op :: _ when
        op <> p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player-> true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _ when
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Border :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Border :: _ when
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true
    | Free :: ps | Border :: ps | Stone _ :: ps -> check ps
    | [] -> false in
  if List.exists check @@ [get_row x gameboard; get_column y gameboard;
                           get_sum_diag (x + y) gameboard; get_diff_diag (x - y) gameboard]
  then Some player
  else None

let start_game size =
  begin
    Random.self_init ();
    Comp_player.clear ();
    Game_gui.display size;
    create_board @@ size + 2
  end

let end_game (winner, moves) =
  begin
    Stat.update_data winner moves;
    Game_gui.return winner
  end

let play_game (Gameboard {size; _} as gameboard) =
  let rec turn gameboard' (Stat.Moves mvs) last_pos player =
    let move_pos =
      match player with
      | Human -> Human_player.move gameboard'
      | Comp -> Comp_player.move last_pos gameboard' in
    let new_moves =
      match player with
      | Human -> Stat.Moves {mvs with human_mv=mvs.human_mv + 1}
      | Comp -> Stat.Moves {mvs with comp_mv=mvs.comp_mv + 1} in
    let new_gameboard = set_move move_pos player gameboard' in
    begin
      Game_gui.draw_stone size player move_pos;
      match check_winner new_gameboard player move_pos with
      | None -> turn new_gameboard new_moves move_pos @@ opponent player
      | Some player -> (player, new_moves)
    end in
  turn gameboard (Stat.Moves {human_mv=0; comp_mv=0}) (0, 0) Human

let run size =
  let gameboard = start_game size in
  let game_result = play_game gameboard in
  end_game game_result
