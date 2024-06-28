
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string body)) in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  match msg with
  | Ast.String s ->
    Format.printf "@.%s@." (Ast.Encoded_string.to_string s)
  | _ ->
    Format.printf "@.%a@."
      Ast.pp_expr msg
