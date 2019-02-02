open Board

let check_winner gameboard size player (row, col) =
  let get_row row' gameboard' = List.nth gameboard' row' in
  let get_column col' gameboard' = List.map (fun lst -> List.nth lst col') gameboard' in
  let get_sum_diag sum gameboard' =
    let rec gs i gameboard'' acc =
      match gameboard'' with
      | [] -> List.rev acc
      | row' :: rows' ->
        if sum - i < 0 || sum - i > size + 1
        then gs (i + 1) rows' acc
        else gs (i + 1) rows' @@ (List.nth row' @@ sum - i) :: acc in
    gs 0 gameboard' [] in
  let get_diff_diag diff gameboard' =
    let rec gd i gameboard'' acc =
      match gameboard'' with
      | [] -> List.rev acc
      | row' :: rows' ->
        if i - diff < 0 || i - diff > size + 1
        then gd (i + 1) rows' acc
        else gd (i + 1) rows' @@ (List.nth row' @@ i - diff) :: acc in
    gd 0 gameboard' [] in
  let rec check lst =
    match lst with
    (* | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Stone p5 :: Free :: _ when
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
        p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p5 = player -> true *)
    | None :: Some p1 :: Some p2 :: Some p3 :: Some p4 :: Some p5 :: None :: _ when
        Some player = p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 -> true
    | Some t0 :: Some p1 :: Some p2 :: Some p3 :: Some p4 :: Some p5 :: None :: _ when
        Some player = p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p1 <> t0 -> true
    | None :: Some p1 :: Some p2 :: Some p3 :: Some p4 :: Some p5 :: Some t6 :: _ when
        Some player = p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p1 <> t6 -> true
    | Some t0 :: Some p1 :: Some p2 :: Some p3 :: Some p4 :: Some p5 :: Some t6 :: _ when
        Some player = p1 && p1 = p2 && p2 = p3 && p3 = p4 && p4 = p5 && p1 <> t0 && p1 <> t6 -> true
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
      | Human -> Human_player.move size gameboard'
      | Comp -> Comp_player.move last_pos size gameboard' in
    let new_moves =
      match player with
      | Human -> Stat.Moves {mvs with human_mv=mvs.human_mv + 1}
      | Comp -> Stat.Moves {mvs with comp_mv=mvs.comp_mv + 1} in
    let new_gameboard = set_move move_pos player gameboard' in
    begin
      Game_gui.draw_stone size player move_pos;
      match check_winner new_gameboard size player move_pos with
      | None -> turn new_moves move_pos (opponent player) new_gameboard
      | Some player -> (player, new_moves)
    end in
  turn (Stat.Moves {human_mv=0; comp_mv=0}) (0, 0) Human gameboard

let run size =
  let gameboard = start_game size in
  let game_result = play_game size gameboard in
  end_game game_result
