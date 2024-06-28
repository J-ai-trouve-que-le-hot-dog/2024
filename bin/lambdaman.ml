type case = Pill | Wall | Empty

type pos = {y: int; x: int}

type game = {
  w: int;
  h: int;
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
      board = Array.init h (fun _ -> Array.make w Wall);
      pos = {x= -1; y= -1} }
  in
  List.iteri (fun y ->
      String.iteri (fun x ->
          let line = game.board.(y) in
          function
          | '#' -> line.(x) <- Wall
          | '.' -> line.(x) <- Pill
          | 'L' -> line.(x) <- Empty; game.pos <- {x;y}
          | c -> Printf.ksprintf failwith "Bad char '%c'" c))
    lines;
  game

let is_win game = Array.for_all (Array.for_all ((<>) Pill)) game.board

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

let move game code =
  let pos = match code with
    | 'U' -> {game.pos with y = game.pos.y - 1}
    | 'D' -> {game.pos with y = game.pos.y + 1}
    | 'L' -> {game.pos with x = game.pos.x - 1}
    | 'R' -> {game.pos with x = game.pos.x + 1}
    | _ -> failwith "bad direction code"
  in
  match get game pos with
  | Wall -> false
  | Pill -> set game pos Empty; game.pos <- pos; true
  | Empty -> game.pos <- pos; true

let check game sol =
  let opt = Buffer.create (String.length sol) in
  String.iter (fun c ->
      move game c |> function
      | true -> Buffer.add_char opt c; (* print game; print_newline () *)
      | false -> Printf.printf "blocked move\n")
    sol;
  if is_win game then Some (Buffer.contents opt) else None




let game =
  let pb = try int_of_string Sys.argv.(1) with _ -> failwith "Arg1: pb number (1..20)" in
  let got = Api.communicate ("S" ^ Ast.encode_string (Printf.sprintf "get lambdaman%d" pb)) in
  match Ast.decode got with
  | Some result -> decode result
  | None -> failwith "TODO eval"

let () = print game

let sol =
  let sol = try Sys.argv.(2) with _ -> failwith "Arg2: Solution string" in
  match check game sol with
  | Some osol when String.length sol = String.length osol ->
    Format.printf "Solution @{<green>GOOD@} (opt)@."
  | Some osol  ->
    Format.printf "Solution @{<green>GOOD@} but could be reduced:@,%s@." osol
  | None -> Format.printf "Solution @{<red>BAD@}@."; exit 1
