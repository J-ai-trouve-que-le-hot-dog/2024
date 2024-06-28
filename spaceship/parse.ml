let positions_from_string s =
  String.split_on_char '\n' s |>
  List.map (fun line ->
      String.split_on_char ' ' line |>
      List.map String.trim |>
      List.filter (function "" -> false | _ -> true)) |>
  List.filter_map (function
      | [] -> None
      | [x; y] -> Some (int_of_string x, int_of_string y)
      | _ -> failwith "Parse error")

let positions s =
  match s with
  | Ast.String s ->
    positions_from_string
      (Ast.Encoded_string.to_string s)
  | _ ->
    let error = Format.asprintf "Unexpected spaceship input@.%a@." Ast.pp_expr s in
    failwith error

let pp ppf l =
  List.iter (fun (x, y) ->
      Format.fprintf ppf "(%i, %i)@ " x y)
    l
