open Board

let check_winner gameboard player (row, col) =
  let get_row row' (Gameboard {fields; _}) = List.nth fields row' in
  let get_column col' (Gameboard {fields;  _}) = List.map (fun lst -> List.nth lst col') fields in
  let get_sum_diag sum (Gameboard {fields; size}) =
    let rec gs i fields' acc =
      match fields' with
      | [] -> List.rev acc
      | row' :: rows' ->
        if sum - i < 0 || sum - i > size + 1
        then gs (i + 1) rows' acc
        else gs (i + 1) rows' @@ (List.nth row' @@ sum - i) :: acc in
    gs 0 fields [] in
  let get_diff_diag diff (Gameboard {fields; size}) =
    let rec gd i fields' acc =
      match fields' with
      | [] -> List.rev acc
      | row' :: rows' ->
        if i - diff < 0 || i - diff > size + 1
        then gd (i + 1) rows' acc
        else gd (i + 1) rows' @@ (List.nth row' @@ i - diff) :: acc in
    gd 0 fields [] in
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
    | _ :: ps -> check ps
    | [] -> false in
  let get_all row' col' gameboard' = [get_row row' gameboard';
                                      get_column col' gameboard';
                                      get_sum_diag (row' + col') gameboard';
                                      get_diff_diag (row' - col') gameboard'] in
  if List.exists check @@ get_all row col gameboard
  then Some player
  else None

let start_game size =
  begin
    Random.self_init ();
    Comp_player.clear ();
    Game_gui.display size;
    create_board @@ size + 2
  end

let end_game (winner, mvs) =
  begin
    Stat.update_data winner mvs;
    Game_gui.return winner
  end

let play_game size gameboard =
  let rec turn (Stat.Moves mvs) last_pos player gameboard' =
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
      | None -> turn new_moves move_pos (opponent player) new_gameboard
      | Some player -> (player, new_moves)
    end in
  turn (Stat.Moves {human_mv=0; comp_mv=0}) (0, 0) Human gameboard

let run size =
  let gameboard = start_game size in
  let game_result = play_game size gameboard in
  end_game game_result
