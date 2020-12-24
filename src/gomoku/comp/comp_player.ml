open Board

type move =
  | Any
  | Comp_five of (int * int)
  | Human_multiple of (int * int)
  | Human_five of (int * int)
  | Comp_multiple of (int * int)
  | Comp_four of (int * int)
  | Human_four of (int * int)

type direction = Row of int | Column of int | Sum of int * int | Diff of int * int

type hole = Five of (int * int) | Four of (int * int)

type move_info = {mutable queue : move list; mutable last : int * int}

let moving = {queue = [Any]; last = (0, 0)}

let get_row_dim n gameboard = (get_row n gameboard, Row n)

let get_column_dim m gameboard = (get_column m gameboard, Column m)

let get_sum_diag_dim sum gameboard =
  (get_sum_diag sum gameboard, Sum (sum, max 0 (sum - gameboard.size - 1)))

let get_diff_diag_dim diff gameboard = (get_diff_diag diff gameboard, Diff (diff, max 0 diff))

let random_element lst = List.nth lst @@ Random.int @@ List.length lst

let compare_positions (v1, p1) (v2, p2) =
  let nc = compare v1 v2 in
  if nc = 0 then compare p1 p2 else -nc

let count_points lst =
  let rec cnt num lst' =
    match lst' with
    | x1 :: (x2 :: _ as xt) -> if x1 = x2 then cnt (num + 1) xt else (num, x1) :: cnt 1 xt
    | [x] -> [(num, x)]
    | [] -> []
  in
  List.sort compare_positions @@ cnt 1 @@ List.sort compare lst

let count_nums lst =
  let rec cnt num lst' =
    match lst' with
    | x1 :: (x2 :: _ as xt) -> if x1 = x2 then cnt (num + 1) xt else (x1, num) :: cnt 1 xt
    | [x] -> [(x, num)]
    | [] -> []
  in
  List.sort compare @@ cnt 1 @@ List.sort compare lst

let get_empties gameboard =
  let neighbours n m =
    [ get_field (GP (n - 1, m - 1)) gameboard; get_field (GP (n - 1, m)) gameboard;
      get_field (GP (n - 1, m + 1)) gameboard; get_field (GP (n, m - 1)) gameboard;
      get_field (GP (n, m + 1)) gameboard; get_field (GP (n + 1, m - 1)) gameboard;
      get_field (GP (n + 1, m)) gameboard; get_field (GP (n + 1, m + 1)) gameboard ]
  in
  let check_stone field =
    match field with
    | Stone _ -> true
    | Border | Free -> false
  in
  let rec mapi_row n m row =
    match row with
    | Free :: fds ->
      if m >= 1 && m <= gameboard.size && (List.exists check_stone @@ neighbours n m)
      then (n, m) :: mapi_row n (m + 1) fds
      else mapi_row n (m + 1) fds
    | Border :: fds | Stone _ :: fds -> mapi_row n (m + 1) fds
    | [] -> []
  in
  let rec mapi_fields n fields' =
    match fields' with
    | row :: rows ->
      if n >= 1 && n <= gameboard.size
      then mapi_row n 0 row :: mapi_fields (n + 1) rows
      else mapi_fields (n + 1) rows
    | [] -> []
  in
  List.concat @@ mapi_fields 0 gameboard.fields

let analyze_situation player (n, m) gameboard =
  let pos_by dir num =
    match dir with
    | Row n' -> (n', num)
    | Column m' -> (num, m')
    | Sum (s, _) -> (num, s - num)
    | Diff (d, _) -> (num, num - d)
  in
  let rec check_at (lst, dir, numrow) acc =
    match lst with
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir numrow in
      check_at (ps, dir, numrow + 5) (Five pos :: acc)
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir (numrow + 1) in
      check_at (ps, dir, numrow + 5) (Five pos :: acc)
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir (numrow + 2) in
      check_at (ps, dir, numrow + 5) (Five pos :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: Stone p4 :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir (numrow + 3) in
      check_at (ps, dir, numrow + 5) (Five pos :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: Free :: ps
      when player = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      let pos = pos_by dir (numrow + 4) in
      check_at (ps, dir, numrow + 5) (Five pos :: acc)
    | Free :: Stone p1 :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir numrow in
      check_at (ps, dir, numrow + 4) (Four pos :: acc)
    | Stone p1 :: Free :: Stone p2 :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir (numrow + 1) in
      check_at (ps, dir, numrow + 4) (Four pos :: acc)
    | Stone p1 :: Stone p2 :: Free :: Stone p3 :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir (numrow + 2) in
      check_at (ps, dir, numrow + 4) (Four pos :: acc)
    | Stone p1 :: Stone p2 :: Stone p3 :: Free :: ps when player = p1 && p1 = p2 && p2 = p3 ->
      let pos = pos_by dir (numrow + 3) in
      check_at (ps, dir, numrow + 4) (Four pos :: acc)
    | Free :: ps | Border :: ps | Stone _ :: ps -> check_at (ps, dir, numrow + 1) acc
    | [] -> acc
  in
  let check (lst, dir) =
    match dir with
    | Row _ | Column _ -> check_at (lst, dir, 0) []
    | Sum (_, p) | Diff (_, p) -> check_at (lst, dir, p) []
  in
  List.concat
  @@ List.map
    check
    [ get_row_dim n gameboard; get_column_dim m gameboard; get_sum_diag_dim (n + m) gameboard;
      get_diff_diag_dim (n - m) gameboard ]

let check_board_situation player gameboard =
  let rec check acc lst =
    match lst with
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: Stone p4 :: ps
      when player = p0 && p0 = p1 && p1 = p2 && p2 = p3 && p3 = p4 ->
      check (5 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: Stone p3 :: ps
      when player = p0 && p0 = p1 && p1 = p2 && p2 = p3 ->
      check (4 :: acc) ps
    | Stone p0 :: Stone p1 :: Stone p2 :: ps when player = p0 && p0 = p1 && p1 = p2 ->
      check (3 :: acc) ps
    | Stone p0 :: Stone p1 :: ps when player = p0 && p0 = p1 -> check (2 :: acc) ps
    | Free :: ps | Border :: ps | Stone _ :: ps -> check acc ps
    | [] -> acc
  in
  let get_rows {fields; _} = fields in
  let get_columns gameboard' = List.mapi (fun i _ -> get_column i gameboard') gameboard'.fields in
  let get_sum_diags gameboard' =
    let rec get_s sum acc =
      if sum <= gameboard'.size * 2
      then get_s (sum + 1) (get_sum_diag sum gameboard' :: acc)
      else acc
    in
    get_s 2 []
  in
  let get_diff_diags gameboard' =
    let rec get_d diff acc =
      if diff <= gameboard'.size - 1
      then get_d (diff + 1) (get_diff_diag diff gameboard' :: acc)
      else acc
    in
    get_d (-gameboard'.size + 1) []
  in
  count_nums @@ List.concat
  @@ List.map (check [])
  @@ List.concat
    [get_rows gameboard; get_columns gameboard; get_sum_diags gameboard; get_diff_diags gameboard]

let make_multiple player situation =
  let sit_points =
    List.map
      (fun x ->
         match x with
         | Five p | Four p -> p )
      situation
  in
  match count_points sit_points with
  | (n, pos) :: _ when n > 1 ->
    ( match player with
      | Human -> Human_multiple pos
      | Comp -> Comp_multiple pos )
  | _ -> Any

let make_five player situation =
  let make_five_list =
    List.filter
      (fun x ->
         match x with
         | Five _ -> true
         | Four _ -> false )
      situation
  in
  match make_five_list with
  | _ :: _ ->
    let (Five pos) = random_element make_five_list in
    ( match player with
      | Human -> Human_five pos
      | Comp -> Comp_five pos )
  | [] -> Any

let make_four player situation =
  let make_four_list =
    List.filter
      (fun x ->
         match x with
         | Five _ -> false
         | Four _ -> true )
      situation
  in
  match make_four_list with
  | _ :: _ ->
    let (Four pos) = random_element make_four_list in
    ( match player with
      | Human -> Human_four pos
      | Comp -> Comp_four pos )
  | [] -> Any

let heura gameboard =
  let comp_sit = check_board_situation Comp gameboard in
  let human_sit = check_board_situation Human gameboard in
  let rec diffs n =
    if n = 0
    then []
    else
      let for_player sit =
        try List.find (fun e -> fst e = n) sit with
        | Not_found -> (n, 0)
      in
      let sit_diff = (snd @@ for_player human_sit) - (snd @@ for_player comp_sit) in
      sit_diff :: diffs (n - 1)
  in
  List.fold_right (fun e a -> float_of_int e +. (1.5 *. a)) (diffs 5) 0.0

let heuristic_move gameboard =
  let cmp f (pm, xm) (pa, xa) =
    if f xm xa
    then (pm, xm)
    else if xm <> xa
    then (pa, xa)
    else if Random.bool ()
    then (pm, xm)
    else (pa, xa)
  in
  let rec forward_move level a b player gameboard' =
    if level = 0
    then ((0, 0), heura gameboard')
    else
      let empty_pos = get_empties gameboard' in
      let rec find_res a' b' lst acc =
        match lst with
        | [] -> acc
        | p :: ps ->
          let gameboard'' = set_move (GP (fst p, snd p)) player gameboard' in
          let acc' = (p, snd @@ forward_move (level - 1) a' b' (opponent player) gameboard'') in
          ( match player with
            | Comp ->
              let acc'' = cmp ( > ) acc' acc in
              let a' = max (snd acc'') a' in
              if a' >= b' then acc'' else find_res a' b' ps acc''
            | Human ->
              let acc'' = cmp ( < ) acc' acc in
              let b' = min (snd acc'') b' in
              if a' >= b' then acc'' else find_res a' b' ps acc'' )
      in
      match player with
      | Comp -> find_res a b empty_pos ((0, 0), neg_infinity)
      | Human -> find_res a b empty_pos ((0, 0), infinity)
  in
  fst @@ forward_move 4 neg_infinity infinity Comp gameboard

let analyze human_move gameboard =
  let analyze' player move =
    let sit = analyze_situation player move gameboard in
    [make_multiple player sit; make_five player sit; make_four player sit]
  in
  List.sort compare @@ analyze' Human human_move @ analyze' Comp moving.last

let clear () =
  moving.queue <- [Any] ;
  moving.last <- (0, 0)

let move (GP (hr, hc)) gameboard =
  let human_move = (hr, hc) in
  let analyzed = analyze human_move gameboard in
  let choose_pos () =
    match List.hd analyzed with
    | Any ->
      ( match List.hd moving.queue with
        | Any -> heuristic_move gameboard
        | Comp_five pos
        | Human_multiple pos
        | Human_five pos
        | Comp_multiple pos
        | Comp_four pos
        | Human_four pos ->
          moving.queue <- List.tl moving.queue ;
          pos )
    | Comp_five pos
    | Human_multiple pos
    | Human_five pos
    | Comp_multiple pos
    | Comp_four pos
    | Human_four pos ->
      let non_any = List.filter (fun mv -> mv <> Any) analyzed in
      moving.queue <- List.rev_append non_any moving.queue ;
      pos
  in
  let rec make_move () =
    let pos = choose_pos () in
    if get_field (GP (fst pos, snd pos)) gameboard = Free then pos else make_move ()
  in
  let move_pos = make_move () in
  moving.last <- move_pos ;
  GP (fst move_pos, snd move_pos)
