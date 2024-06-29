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

type move = U | L | R | D

let move_of_char = function
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | 'R' -> R
  | c -> Printf.ksprintf failwith "Invalid move '%c'" c

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
  List.iter (fun m -> let _: bool = move game m in ()) sol;
  is_win game

let read_file filename =
  let ic = open_in filename in
  let r = In_channel.input_all ic in
  close_in ic;
  r

let run filename str =
  let game_str = read_file filename in
  let game = decode game_str in
  let sol = str_sol str in
  List.iter (fun m -> let _: bool = move game m in ()) sol;
  game
