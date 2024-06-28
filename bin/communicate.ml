
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "S" ^ Ast.encode_string body in
  let result = Api.communicate body in
  match Ast.parse_input result with
  | Ast.String result ->
    Format.printf "String@.%s@." result
  | _ ->
    Format.printf "RAW@.%s@." result
