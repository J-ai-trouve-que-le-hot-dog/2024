type cell =
  | Empty (* empty . *)
  | I of int
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
  | I i -> Printf.sprintf "%i" i
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
    try I (int_of_string s)
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
let set s (i, j) c = s.(i).(j) <- c
let as_int = function I i -> i | _ -> assert false
let of_int i = I i
(* let set_empty s c = set s c Empty *)

type t = cell Array.t Array.t

let empty h w = Array.make_matrix h w Empty
let iterij f (m : t) = Array.iteri (fun i -> Array.iteri (fun j -> f (i, j))) m
let is_operator = function Empty | I _ | Submit | A | B -> false | _ -> true
let is_int = function I _ -> true | _ -> false

let eval ((i, j) as co) s s' =
  let get = get s in
  let set = set s' in
  let c = s.(i).(j) in
  let mk_bin op =
    let l = get (left co) |> as_int in
    let u = get (up co) |> as_int in
    set (down co) (op l u |> of_int);
    set (right co) (op l u |> of_int)
  in
  let mk_eq op =
    let l = get (left co) in
    let u = get (up co) in
    if op l u then (
      set (down co) l;
      set (right co) u)
  in
  match c with
  | Empty | I _ | Submit -> ()
  | Left -> set (left co) (get (right co))
  | Right -> set (right co) (get (left co))
  | Up -> set (up co) (get (down co))
  | Down -> set (down co) (get (up co))
  | Add -> mk_bin ( + )
  | Sub -> mk_bin ( - )
  | Mul -> mk_bin ( * )
  | Div -> mk_bin ( / )
  | Mod -> mk_bin ( mod )
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
  | A -> assert false
  | B -> assert false

let step (s : t) =
  let w, h = width s, height s in
  let new_state = empty w h in
  iterij (fun (i, j) _ -> eval (i, j) s new_state) s;
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
           |> List.map (fun s -> of_string s)
           |> Array.of_list))
    l
  |> Array.of_list

let rec step_n s = function 0 -> s | n -> step_n (step s) (pred n)
let t1 = parse "."
let t1 = parse ". 2 .\n3 + .\n. . ." |> step |> Format.printf "res:@\n%a@." pp
