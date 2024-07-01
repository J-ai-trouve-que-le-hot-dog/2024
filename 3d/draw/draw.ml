
let c_width = 6
module Pos = struct
  type t = int * int
  let compare = compare
end
module Space = Map.Make(Pos)
module PosSet = Set.Make(Pos)

let min_x space =
  Space.fold (fun (x, _) _ acc -> min x acc) space max_int
let min_y space =
  Space.fold (fun (_, y) _ acc -> min y acc) space max_int
let max_x space =
  Space.fold (fun (x, _) _ acc -> max x acc) space min_int
let max_y space =
  Space.fold (fun (_, y) _ acc -> max y acc) space min_int

let pad n s =
  let n = max (0) (n - String.length s) in
  s ^ String.init n (fun _ -> ' ')

let pad_left n s =
  let n = max (0) (n - String.length s) in
  String.init n (fun _ -> ' ') ^ s

type state = {
    space : string Space.t;
    cursor : Pos.t;
    selection : PosSet.t;
    prev : string Space.t list;
  }

let mid space =
  let min_x = min_x space in
  let max_x = max_x space in
  let width = max_x - min_x + 1 in
  let min_y = min_y space in
  let max_y = max_y space in
  let height = max_y - min_y + 1 in
  min_x + width / 2,
  min_y + height / 2

let null_str = String.init c_width (fun _ -> ' ')

let draw state =
  let space = state.space in
  let min_x = min_x space - 2 in
  let max_x = max_x space + 2 in
  let width = max_x - min_x + 1 in
  let min_y = min_y space - 2 in
  let max_y = max_y space + 2 in
  let height = max_y - min_y + 1 in
  Notty.I.tabulate width height
  @@ fun i' j' ->
     let i = i' + min_x in
     let j = j' + min_y in
     let pos = i, j in
     (* Format.printf "%i %i : %i %i@." i' j' i j; *)
     let attr =
       if pos = state.cursor then
         Notty.A.bg Notty.A.green
       else
         if PosSet.mem pos state.selection then
           Notty.A.bg Notty.A.yellow
         else
           Notty.A.bg Notty.A.black
     in
     let s =
       match Space.find_opt (i, j) space with
       | None -> null_str
       | Some s -> pad c_width s
     in
     Notty.I.(string attr s <-> string attr null_str)


let select state =
  let selection =
    if PosSet.mem state.cursor state.selection then
      PosSet.remove state.cursor state.selection
    else
      PosSet.add state.cursor state.selection
  in
  { state with selection }

let (++) (a, b) (c, d) = a+c, b+d

let cursor_move state dir =
  { state with cursor = state.cursor ++ dir }

let logf = open_out "/tmp/caca"
let log fmt = Format.ksprintf (fun s -> output_string logf s; flush logf) fmt

let push state =
  let state = { state with prev = state.space :: state.prev } in
  log "Push %i@." (List.length state.prev);
  state

let selection_move state dir =
  let selection = PosSet.add state.cursor state.selection in
  let removed =
    PosSet.fold (fun pos acc ->
        Space.remove pos acc)
      selection state.space
  in
  let collision =
    PosSet.exists (fun pos ->
        Space.mem (pos ++ dir) removed)
      selection
  in
  if collision then state else begin
      let space =
        PosSet.fold (fun pos acc ->
            match Space.find_opt pos state.space with
            | None -> acc
            | Some v -> Space.add (pos ++ dir) v acc)
          selection removed
      in
      let selection =
        PosSet.map ((++) dir) state.selection
      in
      let state = push state in
      cursor_move { state with space; selection } dir
    end

let left = (-1,0)
let right = (+1,0)
let up = (0,-1)
let down = (0,1)


module P = struct
  (* Copied from 3d/game.ml *)

  type t = string option Array.t Array.t

  let of_string = function
    | "." -> None
    | s -> Some s

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
              if i < Array.length l then l.(i) else None))
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

  let to_space t =
    let acc = ref Space.empty in
    Array.iteri (fun j ->
        Array.iteri (fun i v ->
            match v with
            | None -> ()
            | Some v ->
               acc := Space.add (i, j) v !acc
          )
      ) t;
    !acc

  let read_file filename =
    let ic = open_in filename in
    let _ = input_line ic in
    let rec loop acc =
      try
        let s = input_line ic in
        loop (s :: acc)
      with _ -> String.concat "\n" (List.rev acc)
    in
    let r = loop [] in
    close_in ic;
    r

  let to_array sp =
    let space = sp in
    let min_x = min_x space in
    let max_x = max_x space in
    let width = max_x - min_x + 1 in
    let min_y = min_y space in
    let max_y = max_y space in
    let height = max_y - min_y + 1 in
    let t = Array.init height (fun _ -> Array.init width (fun _ -> None)) in
    for i' = min_x to max_x do
      for j' = min_y to max_y do
        let i = i' - min_x in
        let j = j' - min_y in
        t.(j).(i) <- Space.find_opt (i', j') sp
      done
    done;
    t

end

let to_file =
  if Array.length Sys.argv > 2 then Sys.argv.(2) else
    "/tmp/out.3d"

let file = P.read_file Sys.argv.(1)
let sp = P.to_space (P.parse file)

let init_state = {
    space = sp;
    cursor = mid sp;
    selection = PosSet.empty;
    prev = [];
  }

let output state =
  let oc = open_out to_file in
  let a = P.to_array state.space in
  output_string oc "\n";
  for j = 0 to Array.length a - 1 do
    for i = 0 to Array.length a.(0) - 1 do
      match a.(j).(i) with
      | None -> output_string oc (" " ^ pad_left (c_width) ".")
      | Some s -> output_string oc (" " ^ pad_left (c_width) s)
    done;
    output_string oc "\n";
  done;
  close_out oc

let add_char state char =
  let prev =
    match Space.find_opt state.cursor state.space with
    | None -> ""
    | Some s -> s
  in
  let s' = String.init 1 (fun _ -> char) in
  let s = prev ^ s' in
  if String.length s > c_width then state
  else
    let space = Space.add state.cursor s state.space in
    let state = push state in
    { state with space }

let replace_char state char =
  let s = String.init 1 (fun _ -> char) in
  if String.length s > c_width then state
  else
    let space = Space.add state.cursor s state.space in
    let state = push state in
    { state with space }

let del_char state =
  match Space.find_opt state.cursor state.space with
  | None -> state
  | Some s ->
     let space =
       let s = String.sub s 0 (String.length s - 1) in
       if s = "" then
         Space.remove state.cursor state.space
       else
         Space.add state.cursor s state.space
     in
     let state = push state in
     { state with space }

let undo state =
  match state.prev with
  | [] ->
     log "Pop rien@.";
     state
  | h :: t ->
     log "Pop -> %i@." (List.length t);
     { state with space = h; prev = t }

let delete_selection state =
  { state with selection = PosSet.empty }

let () =
  let open Notty_unix in
  let rec update t state = Term.image t (draw state); loop t state
  and loop t (state : state) =
    match Term.event t with
    | `Key (`Escape,_)        -> ()
    | `Key (`Delete, _) -> update t (delete_selection state)
    | `Key (`ASCII ' ',_)        -> update t (select state)
    | `Key (`ASCII ('z' | 'Z'), [`Ctrl])        ->
       update t (undo state)
    | `Key (`ASCII 's',_)        ->
       output state;
       update t state
    | `Key (`ASCII c,_)        -> update t (add_char state c)
    | `Key (`Backspace,  [])  -> update t (del_char state)
    | `Key (`Arrow `Left, [`Meta])  -> update t (replace_char state '<')
    | `Key (`Arrow `Right, [`Meta]) -> update t (replace_char state '>')
    | `Key (`Arrow `Up, [`Meta])  -> update t (replace_char state '^')
    | `Key (`Arrow `Down, [`Meta]) -> update t (replace_char state 'v')
    | `Key (`Arrow `Left, [])  -> update t (cursor_move state left)
    | `Key (`Arrow `Right, []) -> update t (cursor_move state right)
    | `Key (`Arrow `Up, [])  -> update t (cursor_move state up)
    | `Key (`Arrow `Down, []) -> update t (cursor_move state down)
    | `Key (`Arrow `Left, [`Shift|`Ctrl])  -> update t (selection_move state left)
    | `Key (`Arrow `Right, [`Shift|`Ctrl]) -> update t (selection_move state right)
    | `Key (`Arrow `Up, [`Shift|`Ctrl])  -> update t (selection_move state up)
    | `Key (`Arrow `Down, [`Shift|`Ctrl]) -> update t (selection_move state down)
    | `Resize _              -> update t state
    | _                      -> loop t state
  in
  let t = Term.create () in
  update t init_state;
  Term.release t
