open Board

type direction = Row of int | Column of int | Sum of int * int | Diff of int * int

type move =
  | Comp_make_five of int * int
  | Human_make_more of int * int
  | Human_make_five of int * int
  | Comp_make_more of int * int
  | Comp_make_four of int * int
  | Human_make_four of int * int
  | Any

type hole = Five of (int * int) | Four of (int * int)

let move_queue = ref [Any]

let last_move = ref (0, 0)

let compare_moves m1 m2 =
  match m1, m2 with
  | Any, Any -> 0
  | Any, _ -> 1
  | _, Any -> -1
  | _, _ -> compare m1 m2

let get_row_dim n gameboard =
  (get_row n gameboard, Row n)

let get_column_dim m gameboard =
  (get_column m gameboard, Column m)

let get_sum_diag_dim sum (Gameboard {size; _} as gameboard) =
  (get_sum_diag sum gameboard,
   Sum (sum, max 0 (sum - size - 1)))

let get_diff_diag_dim diff gameboard =
  (get_diff_diag diff gameboard, Diff (diff, max 0 diff))

let random_element lst = List.nth lst @@ Random.int @@ List.length lst

let compare_positions (n1, p1) (n2, p2) =
  let nc = compare n1 n2 in
  if nc = 0 then compare p1 p2 else -nc

let count_points lst =
  let rec cnt num lst' =
    match lst' with
    | x1 :: (x2 :: _ as xt) ->
      if x1 = x2 then cnt (num + 1) xt else (num, x1) :: (cnt 1 xt)
    | [x] -> [(num, x)]
    | [] -> [] in
  List.sort compare_positions @@ cnt 1 @@ List.sort compare lst

let count_nums lst =
  let rec cnt num lst' =
    match lst' with
    | x1 :: (x2 :: _ as xt) ->
      if x1 = x2 then cnt (num + 1) xt else (x1, num) :: (cnt 1 xt)
    | [x] -> [(x, num)]
    | [] -> [] in
  List.sort compare @@ cnt 1 @@ List.sort compare lst

let get_empties (Gameboard {fields; size}) =
  let neighbours row col =
    let prv = List.nth fields (row - 1) in
    let nxt = List.nth fields (row + 1) in
    let same = List.nth fields row in
    [List.nth prv (col - 1); List.nth prv col; List.nth prv (col + 1);
     List.nth same (col - 1); List.nth same (col + 1);
     List.nth nxt (col - 1); List.nth nxt col; List.nth nxt (col + 1)] in
  let check field =
    match field with
    | Stone _ -> true
    | Border | Free -> false in
  let empty i row_i field =
    if i >= 1 && i <= size
    then
      match field with
      | Free -> if List.exists check @@ neighbours row_i i then i else -1
      | Border | Stone _ -> -1
    else -1 in
  let map_row f row_i row =
    let rec mapi_row i row' =
      match row' with
      | [] -> []
      | x :: xs ->
        let res = f i row_i x in
        if res > 0
        then (row_i, res) :: (mapi_row (i + 1) xs)
        else mapi_row (i + 1) xs in
    mapi_row 0 row in
  let row_empty i row =
    if i >= 1 && i <= size
    then map_row empty i row
    else [] in
  List.concat @@ List.mapi row_empty fields

let analyze_situation player (row, col) gameboard =
  let pos_by dir num =
    match dir with
    | Row r -> (r, num)
    | Column c -> (num, c)
    | Sum (s, _) -> (num, s - num)
    | Diff (d, _) -> (num, num - d) in
  let rec check_at (lst, dir, numrow) acc =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps when
        player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir numrow in
      check_at (ps, dir, numrow + 5) @@ (Five pos):: acc
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: Stone p4 :: ps when
        player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir @@ numrow + 1 in
      check_at (ps, dir, numrow + 5) @@ (Five pos):: acc
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: Stone p4 :: ps when
        player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir @@ numrow + 2 in
      check_at (ps, dir, numrow + 5) @@ (Five pos):: acc
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: Stone p4 :: ps when
        player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir @@ numrow + 3 in
      check_at (ps, dir, numrow + 5) @@ (Five pos):: acc
    | Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Free :: ps when
        player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir @@ numrow + 4 in
      check_at (ps, dir, numrow + 5) @@ (Five pos):: acc
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: ps when
        player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir numrow in
      check_at (ps, dir, numrow + 4) @@ (Four pos) :: acc
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: ps when
        player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir @@ numrow + 1 in
      check_at (ps, dir, numrow + 4) @@ (Four pos) :: acc
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: ps when
        player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir @@ numrow + 2 in
      check_at (ps, dir, numrow + 4) @@ (Four pos) :: acc
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: ps when
        player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir @@ numrow + 3 in
      check_at (ps, dir, numrow + 4) @@ (Four pos) :: acc
    | Free :: ps | Border ::ps | Stone _ :: ps -> check_at (ps, dir, numrow + 1) acc
    | [] -> acc in
  let check (lst, dir) =
    match dir with
    | Row _ | Column _ -> check_at (lst, dir, 0) []
    | Sum (_, p) | Diff (_, p) -> check_at (lst, dir, p) [] in
  let get_all r c g = [get_row_dim r g;
                       get_column_dim c g;
                       get_sum_diag_dim (r + c) g;
                       get_diff_diag_dim (r - c) g] in
  List.concat @@ List.map check @@ get_all row col gameboard

let check_board_situation player gameboard =
  let rec check acc lst =
    match lst with
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps when
        player = p0 && p0 = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      check (5 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: ps when
        player = p0 && p0 = p1 && p1 = p2 && p2 = p3 ->
      check (4 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: ps when player = p0 && p0 = p1 && p1 = p2 ->
      check (3 :: acc) ps
    | Stone p0 :: Stone p1 :: ps when player = p0 && p0 = p1 ->
      check (2 :: acc) ps
    | Free :: ps | Border ::ps | Stone _ :: ps -> check acc ps
    | [] -> acc in
  let get_rows (Gameboard {fields; _}) = fields in
  let get_columns (Gameboard {fields; _}) = List.mapi (fun i _ -> List.nth fields i) fields in
  let get_sum_diags (Gameboard {size; _} as gameboard') =
    let rec get_s sum acc =
      if sum <= size + size
      then get_s (sum + 1) @@ (get_sum_diag sum gameboard') :: acc
      else acc in
    get_s 2 [] in
  let get_diff_diags (Gameboard {size; _} as gameboard') =
    let rec get_d diff acc =
      if diff <= size - 1
      then get_d (diff + 1) @@ (get_diff_diag diff gameboard') :: acc
      else acc in
    get_d (-size + 1) [] in
  let get_all gameboard' = List.concat [get_rows gameboard'; get_columns gameboard';
                                        get_sum_diags gameboard'; get_diff_diags gameboard'] in
  count_nums @@ List.concat @@ List.map (check []) @@ get_all gameboard

let numbered player situation =
  let sit_points = List.map (fun x -> match x with Five p | Four p -> p) situation in
  match count_points sit_points with
  | (n, (p1, p2)) :: _ when n > 1 ->
    ( match player with
      | Board.Human -> Human_make_more (p1, p2)
      | Board.Comp -> Comp_make_more (p1, p2)
    )
  | _ -> Any

let make_five player situation =
  let make_five_list =
    List.filter (fun x -> match x with Five _  -> true | Four _ -> false) situation in
  match make_five_list with
  | _ :: _ ->
    let Five (p1, p2) = random_element make_five_list in
    ( match player with
      | Board.Human -> Human_make_five (p1, p2)
      | Board.Comp -> Comp_make_five (p1, p2)
    )
  | [] -> Any

let make_four player situation =
  let make_four_list =
    List.filter (fun x -> match x with Five _  -> false | Four _ -> true) situation in
  match make_four_list with
  | _ :: _ ->
    let Four (p1, p2) = random_element make_four_list in
    ( match player with
      | Board.Human -> Human_make_four (p1, p2)
      | Board.Comp -> Comp_make_four (p1, p2)
    )
  | [] -> Any

let heura gameboard =
  let comp_sit = check_board_situation Board.Comp gameboard in
  let human_sit = check_board_situation Board.Human gameboard in
  let rec diffs n =
    if n = 0
    then []
    else
      let for_player sit = try List.find (fun e -> fst e = n) sit with Not_found -> (n, 0) in
      let sit_diff = (snd @@ for_player human_sit) - (snd @@ for_player comp_sit) in
      sit_diff :: (diffs @@ n - 1) in
  List.fold_right (fun e a -> (float_of_int e) +. 1.5 *. a) (diffs 5) 0.0

let heuristic_move gameboard =
  let cmp f (pm, xm) (pa, xa) =
    if f xm xa
    then (pm, xm)
    else if xm <> xa
    then (pa, xa)
    else if Random.bool ()
    then (pm, xm)
    else (pa, xa) in
  let rec forward_move level a b player gameboard' =
    if level = 0
    then ((0, 0), heura gameboard')
    else
      let empty_pos = get_empties gameboard' in
      let rec find_res a' b' lst acc =
        match lst with
        | [] -> acc
        | p :: ps ->
          let next_gameboard = Board.set_move p player gameboard' in
          let next = forward_move (level - 1) a' b' (Board.opponent player) next_gameboard in
          let nacc = (p, snd next) in
          match player with
          | Board.Comp ->
            let new_acc = cmp (>) nacc acc in
            let new_a = max (snd new_acc) a' in
            if new_a >= b'
            then new_acc
            else find_res new_a b' ps new_acc
          | Board.Human ->
            let new_acc = cmp (<) nacc acc in
            let new_b = min (snd new_acc) b' in
            if a' >= new_b
            then new_acc
            else find_res a' new_b ps new_acc in
      match player with
      | Board.Comp -> find_res a b empty_pos ((0, 0), neg_infinity)
      | Board.Human -> find_res a b empty_pos ((0, 0), infinity) in
  fst @@ forward_move 4 neg_infinity infinity Board.Comp gameboard

let analyze human_move gameboard =
  let analyze' player move =
    let sit = analyze_situation player move gameboard in
    [numbered player sit; make_five player sit; make_four player sit] in
  List.sort compare_moves @@ (analyze' Board.Human human_move) @ (analyze' Board.Comp (!last_move))

let clear () =
  begin
    move_queue := [Any];
    last_move := (0, 0)
  end

let move human_move gameboard =
  let analyzed = analyze human_move gameboard in
  let choose_pos () =
    match List.hd analyzed with
    | Any ->
      ( match List.hd (!move_queue) with
        | Any -> heuristic_move gameboard
        | Comp_make_five (p1, p2)
        | Human_make_more (p1, p2)
        | Human_make_five (p1, p2)
        | Comp_make_more (p1, p2)
        | Comp_make_four (p1, p2)
        | Human_make_four (p1, p2) ->
          begin
            move_queue := List.tl (!move_queue);
            (p1, p2)
          end
      )
    | Comp_make_five (p1, p2)
    | Human_make_more (p1, p2)
    | Human_make_five (p1, p2)
    | Comp_make_more (p1, p2)
    | Comp_make_four (p1, p2)
    | Human_make_four (p1, p2) ->
      let non_any = List.filter (fun m -> m <> Any) analyzed in
      begin
        move_queue := List.rev_append non_any (!move_queue);
        (p1, p2)
      end in
  let rec make_move () =
    let pos = choose_pos () in
    if Board.is_free pos gameboard
    then pos
    else make_move () in
  let move_pos = make_move () in
  begin
    last_move := move_pos;
    move_pos
  end
