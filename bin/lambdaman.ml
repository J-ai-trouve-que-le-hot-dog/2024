type case = Pill | Wall | Empty

type pos = {y: int; x: int}

type game = {
  w: int;
  h: int;
  mutable npills: int;
  board: case array array;
  mutable pos: pos;
}

let decode s =
  let lines = String.split_on_char '\n' (String.trim s) in
  let w = String.length (List.hd lines) in
  let h = List.length lines in
  let game =
    { w;
      h;
      npills = 0;
      board = Array.init h (fun _ -> Array.make w Wall);
      pos = {x= -1; y= -1} }
  in
  List.iteri (fun y ->
      String.iteri (fun x ->
          let line = game.board.(y) in
          function
          | '#' -> line.(x) <- Wall
          | '.' -> line.(x) <- Pill; game.npills <- game.npills + 1
          | 'L' -> line.(x) <- Empty; game.pos <- {x;y}
          | c -> Printf.ksprintf failwith "Bad char '%c'" c))
    lines;
  game

let is_win game = game.npills = 0

let get game pos =
  try game.board.(pos.y).(pos.x) with Invalid_argument _ -> Wall

let set game pos c =
  game.board.(pos.y).(pos.x) <- c

let print game =
  for y = 0 to game.h - 1 do
    for x = 0 to game.w - 1 do
      print_char @@
      match get game { x; y} with
      | Pill -> '.'
      | Empty -> ' '
      | Wall -> '#'
    done;
    print_newline ()
  done;
  flush stdout

type move = U | D | L | R

let move_of_char = function
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | 'R' -> R
  | c -> Printf.ksprintf failwith "Invalid move '%c'" c

let char_of_move = function
  | U -> 'U'
  | D -> 'D'
  | L -> 'L'
  | R -> 'R'

let sol_str sol = List.to_seq sol |> Seq.map char_of_move |> String.of_seq

let str_sol str = String.to_seq str |> Seq.map move_of_char |> List.of_seq

let app_move pos = function
  | U -> {pos with y = pos.y - 1}
  | D -> {pos with y = pos.y + 1}
  | L -> {pos with x = pos.x - 1}
  | R -> {pos with x = pos.x + 1}

let move game m =
  let pos = app_move game.pos m in
  match get game pos with
  | Wall -> false
  | Pill -> set game pos Empty; game.npills <- game.npills - 1; game.pos <- pos; true
  | Empty -> game.pos <- pos; true

let check game sol =
  let ropt = ref [] in
  List.iter (fun m ->
      move game m |> function
      | true -> ropt := m :: !ropt (* ; print game; print_newline () *)
      | false -> Printf.printf "blocked move\n")
    sol;
  if is_win game then Some (List.rev !ropt) else None

let neighbours game pos =
  List.filter_map (fun m ->
      let pos = app_move pos m in
      match get game pos with
      | Wall -> None
      | Pill -> Some (pos,m,true)
      | Empty -> Some (pos,m,false))
    [U;D;L;R]

module PMap = Map.Make(struct type t = pos let compare = compare end)

let rec auto rsol game =
  (* let rec neigh lastpos pos =
   *   let dests = neighbours game pos in
   *   let pills, emptys = List.partition (fun (_,_,x) -> x) dests in
   *   if pills <> [] then `L pills
   *   else `R emptys *)
  print_char '.';
  let rpaths =
    let rec bfs seen poss =
      let (pills, emptys) =
        PMap.fold (fun p rpath (pills, emptys) ->
            let dests = neighbours game p |> List.to_seq in
            let pills', emptys' =
              Seq.partition_map (fun (p,m,ispill) ->
                  if ispill then Left (p, m::rpath)
                  else Right (p, m::rpath)) dests
            in
            let pills = PMap.add_seq pills' pills in
            if PMap.is_empty pills then pills, PMap.add_seq emptys' emptys
            else pills, PMap.empty)
          poss (PMap.empty, PMap.empty)
      in
      if PMap.is_empty pills then
        bfs
          (PMap.union (fun _ x _ -> Some x) seen emptys)
          (PMap.merge (fun _ l -> function None -> l | Some _ -> None) emptys seen)
      else pills
    in
    bfs (PMap.singleton game.pos []) (PMap.singleton game.pos [])
  in
  let _, rpath = PMap.choose rpaths in
  let path = List.rev rpath in
  let rsol = List.rev_append path rsol in
  List.iter (fun m -> move game m |> ignore) path;
  if is_win game then
    let sol = List.rev rsol in
    Format.printf "@,@{<yellow>Solved!@}@,%s@," (sol_str sol);
    sol
  else
    auto rsol game

let pb_num =
  try int_of_string Sys.argv.(1) with _ -> failwith "Arg1: pb number (1..20)"

let game_str =
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string ((Printf.sprintf "get lambdaman%d" pb_num)))) in
  let got = Api.communicate body in
  Eval.eval EnvEmpty (Eval.term_from_expr (Ast.parse_input got))
  |> Format.asprintf "%a" Eval.pp_value

let main () =
  let game = decode game_str in
  print game;
  if Array.length Sys.argv >= 3 then
    let ssol = try Sys.argv.(2) with _ -> failwith "Arg2: Solution string" in
    let sol = str_sol ssol in
    match check game sol with
    | Some osol when List.length sol = List.length osol ->
      Format.printf "Solution @{<green>GOOD@} (opt)@."
    | Some osol  ->
      Format.printf "Solution @{<green>GOOD@} but could be reduced:@,%s@."
        (sol_str osol)
    | None -> Format.printf "Solution @{<red>BAD@}@."; exit 1
  else
    let sol = auto [] game in
    let sol =
      match check (decode game_str) sol with
      | Some osol when List.length sol = List.length osol ->
        Format.printf "Solution @{<green>GOOD@} (opt)@."; sol
      | Some osol  ->
        Format.printf "Solution @{<green>GOOD@} but could be reduced:@,%s@."
          (sol_str osol); osol
      | None -> Format.printf "Solution @{<red>BAD@}@."; exit 1
    in
    let sol = Miniterm.lambdaman_solution pb_num (sol_str sol) in
    let answer =
      Api.communicate (Format.asprintf "%a" Ast.print_ast sol)
    in
    let r = Eval.eval EnvEmpty (Eval.term_from_expr (Ast.parse_input answer)) in
    Format.printf "ANSWER:@,%a@," Eval.pp_value r


let () =
  Format.open_vbox 0;
  main ();
  Format.close_box ();
  Format.print_flush ()
