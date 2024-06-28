
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "S" ^ Ast.encode_string body in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  Format.printf "@.%a@."
    Ast.pp_expr msg
