
type var = V of string

type op = char

type input =
  | Var of var * string
  | Const of string

type output = var


type elt = { op : op; l : input; u : input; out : output }

type copy = { outs : var list; copied : var }

type program = {
    act : elt list;
    copies : copy list;
    outputs : var list;
  }



module Pos = struct
  type t = int * int
  let compare = compare
end
module Space = Map.Make(Pos)
let min_x space =
  Space.fold (fun (x, _) _ acc -> min x acc) space max_int
let min_y space =
  Space.fold (fun (_, y) _ acc -> min y acc) space max_int
let max_x space =
  Space.fold (fun (x, _) _ acc -> max x acc) space min_int
let max_y space =
  Space.fold (fun (_, y) _ acc -> max y acc) space min_int

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



type out = {
    mutable acc : string Space.t;
    mutable first_free_y : int;
  }

let add_out out (i, j) s =
  out.acc <- Space.add (i, j+out.first_free_y) s out.acc

let var_label (V v) = v

let string_of_input = function
  | Var (v, init) -> var_label v ^ ":" ^ init
  | Const s -> s

let output_var_at (out: out) outputs (i, j) v =
  let at = add_out out in
  if List.mem v outputs then
    at (i, j) "S"
  else begin
      at (i+1, j-1) (var_label v ^ "@x");
      at (i+1, j) "@";
      at (i+1, j+1) (var_label v ^ "@y");
      at (i+2, j) "1"
    end

let output_elt (out:out) outputs elt =
  let at = add_out out in
  at (0, 1) (string_of_input elt.u);
  at (1, 0) (string_of_input elt.l);
  at (1, 1) (String.make 1 elt.op);
  output_var_at out outputs (2, 1) elt.out;
  out.first_free_y <- out.first_free_y + 3

let output_copy (out:out) outputs elt =
  let at = add_out out in
  let (out1, out2) = match elt.outs with [a;b] -> (a,b) | _ -> assert false in
  at (0, 3) (var_label elt.copied ^ ":");
  at (0, 2) "<";
  at (0, 4) ">";
  output_var_at out outputs (0, 1) out1;
  output_var_at out outputs (0, 5) out2;
  out.first_free_y <- out.first_free_y + 7

let output_prog prog =
  let out = { acc = Space.empty ; first_free_y = 0 } in
  List.iter (output_elt out prog.outputs) prog.act;
  List.iter (output_copy out prog.outputs) prog.copies;
  P.to_array out.acc


let pad_left n s =
  let n = n - String.length s in
  String.init n (fun _ -> ' ') ^ s

let c_width = 5

let output a =
  print_string "\n";
  for i = 0 to Array.length a.(0) - 1 do
    for j = 0 to Array.length a - 1 do
      match a.(j).(i) with
      | None -> print_string (" " ^ pad_left (c_width) ".")
      | Some s -> print_string (" " ^ pad_left (c_width) s)
    done;
    print_string "\n";
  done

