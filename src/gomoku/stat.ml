type stat_info =
  {hmoves : int; cmoves : int; won : int; lost : int; thmoves : int; tcmoves : int; opened : int}

type moves_count = {human_mv : int; comp_mv : int}

exception Stat_format_error of string

let filename = ".gomoku.stat"

let case_base () = if Random.bool () then 'A' else 'a'

let encode_num num =
  let enc num' res =
    if num' = 0
    then String.make 1 @@ case_base ()
    else
      let rec enc' num'' res' =
        if num'' = 0
        then res'
        else
          let n = num'' mod 10 in
          let base = Char.code @@ case_base () in
          let str = String.make 1 @@ Char.chr ((2 * n) + base) in
          enc' (num'' / 10) @@ str ^ res'
      in
      enc' num' res
  in
  enc num ""

let encode stat_rcd =
  let list_of_stat {hmoves; cmoves; won; lost; thmoves; tcmoves; opened} =
    [hmoves; cmoves; won; lost; thmoves; tcmoves; opened]
  in
  let rec concatmap lst res =
    match lst with
    | [] -> res
    | [x] -> res ^ encode_num x
    | x :: xs ->
      let base = Char.code @@ case_base () in
      let sep = String.make 1 @@ Char.chr ((2 * Random.int 16) + base - 3) in
      concatmap xs @@ res ^ encode_num x ^ sep
  in
  concatmap (list_of_stat stat_rcd) ""

let decode str =
  let rec split str' i act res =
    if i = String.length str'
    then List.rev act :: res
    else
      let cd = Char.code str'.[i] mod 32 in
      if cd mod 2 = 1 && cd < 20
      then split str' (i + 1) ((cd / 2) :: act) res
      else
        match act with
        | [] -> split str' (i + 1) [] res
        | _ -> split str' (i + 1) [] (List.rev act :: res)
  in
  let rec make_int res lst =
    match lst with
    | x :: xs -> make_int ((res * 10) + x) xs
    | [] -> res
  in
  let stat_from_list lst =
    match lst with
    | [hmoves_num; cmoves_num; won_num; lost_num; thmoves_num; tcmoves_num; opened_num] ->
      { hmoves = make_int 0 hmoves_num;
        cmoves = make_int 0 cmoves_num;
        won = make_int 0 won_num;
        lost = make_int 0 lost_num;
        thmoves = make_int 0 thmoves_num;
        tcmoves = make_int 0 tcmoves_num;
        opened = make_int 0 opened_num }
    | _ -> raise @@ Stat_format_error "Stat.read"
  in
  stat_from_list @@ List.rev @@ split str 0 [] []

let write stat =
  let text = encode stat in
  let file = open_out filename in
  output_string file text ; flush file ; close_out file

let clear () =
  write {hmoves = 0; cmoves = 0; won = 0; lost = 0; thmoves = 0; tcmoves = 0; opened = 0}

let read () =
  let file =
    try open_in filename with
    | Sys_error _ -> clear () ; open_in filename
  in
  let text = input_line file in
  close_in file ; decode text

let update_data winner {human_mv; comp_mv} =
  let {won; lost; thmoves; tcmoves; opened; _} = read () in
  match winner with
  | Board.Human ->
    write
      { hmoves = human_mv;
        cmoves = comp_mv;
        won = won + 1;
        lost;
        thmoves = thmoves + human_mv;
        tcmoves = tcmoves + comp_mv;
        opened }
  | Board.Comp ->
    write
      { hmoves = human_mv;
        cmoves = comp_mv;
        won;
        lost = lost + 1;
        thmoves = thmoves + human_mv;
        tcmoves = tcmoves + comp_mv;
        opened }

let prepare_data () =
  let rcd = read () in
  write {rcd with opened = rcd.opened + 1}
