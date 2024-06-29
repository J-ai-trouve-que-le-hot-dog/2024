let () = Ocolor_format.prettify_formatter Format.std_formatter

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

let format ppf = function
  | Empty -> Format.fprintf ppf "@{<black;bold>.@}"
  | I i -> Format.fprintf ppf "@{<yellow>%a@}" Z.pp_print i
  | Left -> Format.fprintf ppf "@{<cyan><@}"
  | Right -> Format.fprintf ppf "@{<cyan>>@}"
  | Up -> Format.fprintf ppf "@{<cyan>^@}"
  | Down -> Format.fprintf ppf "@{<cyan>v@}"
  | Add -> Format.fprintf ppf "@{<blue>+@}"
  | Sub -> Format.fprintf ppf "@{<blue>-@}"
  | Mul -> Format.fprintf ppf "@{<blue>*@}"
  | Div -> Format.fprintf ppf "@{<blue>/@}"
  | Mod -> Format.fprintf ppf "@{<blue>%%@}"
  | Warp -> Format.fprintf ppf "@{<red;bold>@@@}"
  | Eq -> Format.fprintf ppf "@{<blue>=@}"
  | Neq -> Format.fprintf ppf "@{<blue>#@}"
  | Submit -> Format.fprintf ppf "@{<green>S@}"
  | A -> Format.fprintf ppf "@{<green>A@}"
  | B -> Format.fprintf ppf "@{<green>B@}"


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

exception Timewarp of (int * (int * int) * cell)

let eval n (h : (int * int, cell) Hashtbl.t) ((i, j) as co) s s' =
  let get = get s in
  let is_nempty c = get c <> Empty in
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
        set (right co) u;
        set_empty (left co);
        set_empty (up co))
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
    | Warp -> (
      let u = up co in
      let l = left co in
      let r = right co in
      let d = down co in
      if List.for_all is_nempty [u; l; r; d] then
        try
          let dt = get d |> as_int in
          let dx = get l |> as_int in
          let dy = get r |> as_int in
          let v = get u in
          let open Z.Compare in
          if dt < Z.one then
            Format.kasprintf failwith "timewarp bad dt: %a" Z.pp_print dt;
          let c = i - Z.to_int dy, j - Z.to_int dx in
          raise (Timewarp (n - Z.to_int dt, c, v))
        with
        | Timewarp _ as e ->
          Format.printf "%s@." __LOC__;
          raise e
        | _ ->
          Format.ksprintf failwith
            "timewarp bad input value: dt:%s, dx:%s, dy:%s"
            (to_string (get d))
            (to_string (get l))
            (to_string (get r)))
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
  with
  | Timewarp _ as e -> raise e
  | e -> Format.printf "%s at (%d,%d)@." (Printexc.to_string e) i j

let copy m = Array.map Array.copy m

exception Fini of cell

let step n states (s : t) : int * t =
  let new_state = copy s in
  let h = Hashtbl.create 15 in
  let timewarps = ref [] in
  iterij
    (fun (i, j) _ ->
      try eval n h (i, j) s new_state
      with Timewarp x -> timewarps := x :: !timewarps)
    s;
  let rec check_timewarps dt = function
    | [] -> true
    | (dt', _, _) :: t -> (
      match dt with
      | None -> check_timewarps (Some dt') t
      | Some dt -> dt = dt' && check_timewarps (Some dt') t)
  in
  let fini =
    Hashtbl.fold
      (fun c v acc ->
         if get new_state c = Submit then v :: acc else acc)
      h []
  in
  let () = match fini with
    | [] -> ()
    | [v] -> raise (Fini v)
    | v::vs -> if List.for_all ((=) v) vs then raise (Fini v)
      else failwith "Multiple Submit"
  in
  assert (check_timewarps None !timewarps);
  if !timewarps <> [] then (
    let new_id, _, _ = List.hd !timewarps in
    Format.printf "Timewarping to %d!@." new_id;
    let new_state = Hashtbl.find states new_id in
    List.iter (fun (_, c, v) -> set new_state c v) !timewarps;
    new_id, new_state)
  else (
    Hashtbl.iter (set new_state) h;
    succ n, new_state)

let pp fmt m =
  let width = function
    | I x -> String.length (Z.to_string x)
    | _ -> 1
  in
  Array.iter
    (fun l ->
      Array.iteri (fun i ->
           let w =
             Array.fold_left (fun acc l ->
                 try max (width l.(i)) acc
                 with Invalid_argument _ -> acc)
               1
               m
           in
           function
           | I x -> Format.fprintf fmt "@{<yellow>%*s@} " w (Z.to_string x)
           | c -> Format.fprintf fmt "%*s%a " (w-1) "" format c)
        l;
      Format.fprintf fmt "@\n")
    m

let pad t =
  let maxw =
    Array.fold_right (fun l -> max (Array.length l))
      t 0
  in
  Array.map (fun l ->
      let w = Array.length l in
      if w = maxw then l
      else
        Array.init maxw (fun i ->
            if i < Array.length l then l.(i) else Empty))
    t

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
  |> pad

(* let rec step_n s = function 0 -> s | n -> step_n (step s) (pred n) *)

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
  let h = Hashtbl.create 10 in
  Hashtbl.replace h 1 p;
  let rec loop pred_id i x states =
    Format.printf "%s%d:@\n%a@."
      (match pred_id with
      | None -> ""
      | Some pred_id -> Printf.sprintf "%d -> " pred_id)
      i pp x;
    let _ = read_line () in
    let new_id, new_state = step i states x in
    Hashtbl.add states new_id new_state;
    loop (Some i) new_id new_state states
  in
  try loop None 1 p h with Fini c -> Format.printf "Eval: %s@." (to_string c)

let () =
  main ()
