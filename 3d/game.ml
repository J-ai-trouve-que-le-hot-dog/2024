type cell =
  | Empty (* empty . *)
  | I of Z.t
  | Left (* < *)
  | Right (* > *)
  | Up (* ^ *)
  | Down (* v *)
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Mod (* % *)
  | Warp (* @ *)
  | Eq (* = *)
  | Neq (* # *)
  | Submit (* S *)
  | A
  | B

let to_string = function
  | Empty -> "."
  | I i -> Format.asprintf "%a" Z.pp_print i
  | Left -> "<"
  | Right -> ">"
  | Up -> "^"
  | Down -> "v"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Warp -> "@"
  | Eq -> "="
  | Neq -> "#"
  | Submit -> "S"
  | A -> "A"
  | B -> "B"

let of_string = function
  | "." -> Empty
  | "<" -> Left
  | ">" -> Right
  | "^" -> Up
  | "v" -> Down
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | "@" -> Warp
  | "=" -> Eq
  | "#" -> Neq
  | "S" -> Submit
  | "A" -> A
  | "B" -> B
  | s -> (
    try I (Z.of_string s)
    with e ->
      Format.printf "exn on '%s'@." s;
      raise e)

let width m = Array.length m.(0)
let height m = Array.length m

let product a b =
  let rec loop acc = function
    | [] -> acc
    | hd :: tl ->
      let acc' = List.rev_append (List.rev_map (fun x -> hd, x) b) acc in
      loop acc' tl
  in
  loop [] a

let up (i, j) = i - 1, j
let down (i, j) = i + 1, j
let left (i, j) = i, j - 1
let right (i, j) = i, j + 1
let get s (i, j) = s.(i).(j)

let set s (i, j) c =
  Format.printf "s.(%d).(%d) <- %s@." i j (to_string c);
  s.(i).(j) <- c

let set_empty s (i, j) = s.(i).(j) <- Empty
let as_int = function I i -> i | _ -> assert false
let of_int i = I i
(* let set_empty s c = set s c Empty *)

type t = cell Array.t Array.t

let empty h w = Array.make_matrix h w Empty
let iterij f (m : t) = Array.iteri (fun i -> Array.iteri (fun j -> f (i, j))) m
let is_operator = function Empty | I _ | Submit | A | B -> false | _ -> true
let is_int = function I _ -> true | _ -> false

let eval (h : (int * int, cell) Hashtbl.t) ((i, j) as co) s s' =
  let get = get s in
  let set p c =
    match Hashtbl.find_opt h p with
    | None | Some Empty -> Hashtbl.replace h p c
    | Some c' ->
      if c = c' then Hashtbl.replace h p c
      else
        Format.ksprintf failwith "bad set: %s vs. %s" (to_string c)
          (to_string c')
  in
  let set_empty = set_empty s' in
  let c = s.(i).(j) in
  let mk_bin op =
    try
      let l = get (left co) |> as_int in
      let u = get (up co) |> as_int in
      set (down co) (op l u |> of_int);
      set (right co) (op l u |> of_int);
      set_empty (left co);
      set_empty (up co)
    with _ -> ()
  in
  let mk_eq op =
    try
      let l = get (left co) in
      let u = get (up co) in
      if op l u then (
        set (down co) l;
        set (right co) u);
      set_empty (left co);
      set_empty (up co)
    with _ -> ()
  in
  try
    match c with
    | I _ | Empty | A | B | Submit -> ()
    | Left ->
      if not (get (right co) = Empty) then (
        set (left co) (get (right co));
        set_empty (right co))
    | Right ->
      if not (get (left co) = Empty) then (
        set (right co) (get (left co));
        set_empty (left co))
    | Up ->
      if not (get (down co) = Empty) then (
        set (up co) (get (down co));
        set_empty (down co))
    | Down ->
      if not (get (up co) = Empty) then (
        set (down co) (get (up co));
        set_empty (up co))
    | Add -> mk_bin Z.add
    | Sub -> mk_bin Z.sub
    | Mul -> mk_bin Z.mul
    | Div -> mk_bin Z.div
    | Mod -> mk_bin Z.rem
    | Warp -> failwith "toudou"
    | Eq ->
      let l = get (left co) in
      let u = get (up co) in
      if (is_int l && is_int u) || (is_operator l && is_operator u) then
        mk_eq ( = )
    | Neq ->
      let l = get (left co) in
      let u = get (up co) in
      if (is_int l && is_int u) || (is_operator l && is_operator u) then
        mk_eq ( <> )
  with e -> Format.printf "%s at (%d,%d)@." (Printexc.to_string e) i j

let copy m = Array.map Array.copy m

exception Fini of cell

let step (s : t) =
  let new_state = copy s in
  let h = Hashtbl.create 15 in
  iterij (fun (i, j) _ -> eval h (i, j) s new_state) s;
  Hashtbl.iter
    (fun c v ->
      if get new_state c = Submit then raise (Fini v);
      set new_state c v)
    h;
  new_state

let pp fmt m =
  Array.iter
    (fun l ->
      Array.iter (fun c -> Format.fprintf fmt "%s " (to_string c)) l;
      Format.fprintf fmt "@\n")
    m

let parse s : t =
  let l = String.split_on_char '\n' s in
  List.filter_map
    (function
      | "" -> None
      | s ->
        Option.some
        @@ (String.trim s
           |> String.split_on_char ' '
           |> List.map String.trim
           |> List.filter (function "" -> false | _ -> true)
           |> List.map (fun s -> of_string s)
           |> Array.of_list))
    l
  |> Array.of_list

let rec step_n s = function 0 -> s | n -> step_n (step s) (pred n)
let t1 = parse "."
let t1 = parse ". 2 .\n3 + .\n. . ." |> step |> Format.printf "res:@\n%a@." pp

let read_file ic =
  let rec loop acc =
    try
      let s = input_line ic in
      loop (s :: acc)
    with _ -> String.concat "\n" (List.rev acc)
  in
  loop []

let map f (m : t) = Array.map (Array.map f) m

let main () =
  let ic = open_in Sys.argv.(1) in
  let a = Sys.argv.(2) |> Z.of_string in
  let b = Sys.argv.(3) |> Z.of_string in
  let _ = input_line ic in
  let p =
    read_file ic |> parse |> map (function A -> I a | B -> I b | x -> x)
  in
  let rec loop i x =
    Format.printf "%d:@\n%a@." i pp x;
    let _ = read_line () in
    loop (succ i) (step x)
  in
  try loop 1 p with Fini c -> Format.printf "Eval: %s@." (to_string c)

let () =
  try main ()
  with e ->
    Format.printf "exn: %s@." (Printexc.to_string e);
    Format.printf "usage: ./game.exe <file> <A> <B>"
