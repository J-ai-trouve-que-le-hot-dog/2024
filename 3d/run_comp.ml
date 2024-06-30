open Compile

type value = Z.t
type env = (var * value) list

let string_value s a b =
  match s with
  | "" -> None
  | "A" -> Some a
  | "B" -> Some b
  | _ ->
    let v =
      try Z.of_int @@ int_of_string s
      with _ -> failwith (Format.sprintf "Not an integer %s" s)
    in
    Some v

let init_program a b program : env =
  let string_value s = string_value s a b in
  let init_copy =
    List.fold_left
      (fun acc { copied = var, s; _ } ->
        match string_value s with None -> acc | Some v -> (var, v) :: acc)
      [] program.copies
  in
  List.fold_left
    (fun acc { l; u; _ } ->
      let add acc = function
        | Const _ -> acc
        | Var (var, s) -> (
          match string_value s with None -> acc | Some v -> (var, v) :: acc)
      in
      let acc = add acc l in
      let acc = add acc u in
      acc)
    init_copy program.act

let step a b values program =
  let new_values =
    List.fold_left (fun new_values op ->
        let { op; l; u; out } = op in
        let get = function
          | Const s -> string_value s a b
          | Var (v, _) -> List.assoc_opt v values
        in
        match get l, get u with
        | None, _ | _, None -> new_values
        | Some l, Some u -> (
          let res =
            match op with
            | '+' -> Some (Z.add l u)
            | '*' -> Some (Z.mul l u)
            | '-' -> Some (Z.sub l u)
            | '/' -> begin
                if Z.equal u Z.zero then
                  None
                else
                  Some (Z.div l u)
              end
            | '=' -> if Z.equal l u then Some l else None
            | _ -> failwith "TODO"
          in
          match res with
          | None -> new_values
          | Some res -> (
            match List.assoc_opt out new_values with
            | Some _ ->
              let (V out) = out in
              failwith (Printf.sprintf "Writing twice in %s" out)
            | None -> (out, res) :: new_values)))
      [] program.act
  in
  let new_values =
    List.fold_left (fun new_values { outs; copied } ->
        let (var, _) = copied in
        match List.assoc_opt var values with
        | None -> new_values
        | Some res ->
          List.fold_left (fun new_values out ->
              match List.assoc_opt out new_values with
              | Some _ ->
                let (V out) = out in
                failwith (Printf.sprintf "Writing twice in %s (copy)" out)
              | None -> (out, res) :: new_values)
            new_values outs)
      new_values program.copies
  in
  List.fold_left (fun acc (var, value) ->
      let acc = List.remove_assoc var acc in
      (var, value) :: acc)
    values new_values

let result values program =
  List.filter_map (fun var ->
      List.assoc_opt var values)
      program.outputs

let display i values =
  Format.printf "STEP: %d@." i;
  List.iter (fun ((V var), value) ->
      Format.printf "%5s: %a@." var Z.pp_print value)
    values;
  Format.printf "@.@."

let run a b program =
  let a = Z.of_int a in
  let b = Z.of_int b in
  let init = init_program a b program in
  let rec loop i values =
    let values = step a b values program in
    display i values;
    match result values program with
    | [] -> loop (i+1) values
    | [v] -> v
    | v :: t ->
      if List.for_all (fun v' -> Z.equal v v') t then
        v
      else
        failwith (Format.asprintf "Multiple outputs %a %a" Z.pp_print v Z.pp_print (List.hd t))
  in
  display 0 init;
  let result = loop 1 init in
  Format.printf "RESULT:@.%a@.@." Z.pp_print result
