open Compile

type value = Z.t
type env = (var * value) list

let pp_z ff z =
  let s = Z.to_string z in
  if String.length s > 20 then
    begin
      if Z.gt z Z.zero && Z.popcount z <= 8 then begin
        let r = ref z in
        while not (Z.equal !r Z.zero) do
          let n = Z.trailing_zeros !r in
          r := Z.sub !r (Z.shift_left Z.one n); 
          Format.fprintf ff "2^%d" n;
          if not (Z.equal !r Z.zero) then Format.fprintf ff "+" 
        done;
      end else
        Format.fprintf ff "%s[...]%s" (String.sub s 0 5) (String.sub s (String.length s - 5) 5)
    end
  else Format.fprintf ff "%s" s

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

let step cur_step a b values inited program =
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
            | '%' -> begin
                if Z.equal u Z.zero then
                  None
                else
                  Some (Z.rem l u)
              end
            | '=' -> if Z.equal l u then Some l else None
            | '#' -> if Z.equal l u then None else Some l
            | _ -> failwith (Printf.sprintf "TODO %c" op)
          in
          match res with
          | None -> new_values
          | Some res -> (
            match List.assoc_opt out new_values with
            | Some value ->
              let (V out) = out in
              failwith (Format.asprintf "Writing twice in %s %a %a" out pp_z value pp_z res)
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
              | Some value ->
                let (V out) = out in
                failwith (Format.asprintf "Writing twice in %s %a %a (copy)" out pp_z value pp_z res)
              | None -> (out, res) :: new_values)
            new_values outs)
      new_values program.copies
  in
  let new_values, inited =
    List.fold_left (fun (new_values, inited) { var_to_init; init_from } ->
        if not (List.mem var_to_init inited) then
          match List.assoc_opt init_from values with
          | None -> (new_values, inited)
          | Some value ->
            ((var_to_init, value) :: new_values, var_to_init :: inited)
        else
          (new_values, inited)
      ) (new_values, inited)
      program.to_init_from
  in
  let values =
  List.fold_left (fun acc (var, value) ->
    if not (List.mem_assoc var acc) then Format.eprintf "VAR %s produced at %d@." (let (V v) = var in v) cur_step;
      let acc = List.remove_assoc var acc in
      (var, value) :: acc)
    values new_values
  in
  values, inited

let result values program =
  List.filter_map (fun var ->
      List.assoc_opt var values)
      program.outputs

let display i values =
  Format.printf "STEP: %d@." i;
  List.iter (fun ((V var), value) ->
      if not (String.starts_with ~prefix:"Dm" var) then
        Format.printf "%5s: %a@." var pp_z value)
    values;
  Format.printf "@.@."

let run ?(max=max_int) a b program =
  let a = Z.of_int a in
  let b = Z.of_int b in
  let init = init_program a b program in
  let rec loop i values inited =
    if i > max then None
    else
    let values, inited = step i a b values inited program in
    display i values;
    match result values program with
    | [] -> loop (i+1) values inited
    | [v] -> Some v
    | v :: t ->
      if List.for_all (fun v' -> Z.equal v v') t then
        Some v
      else
        failwith (Format.asprintf "Multiple outputs %a %a" pp_z v pp_z (List.hd t))
  in
  display 0 init;
  let result = loop 1 init [] in
  match result with
  | None -> Format.printf "STOPPED@."
  | Some result ->
    Format.printf "RESULT:@.%a@.@." pp_z result
