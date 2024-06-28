
let () =
  assert(Array.length Sys.argv = 2);
  let body = Sys.argv.(1) in
  let body = "S" ^ Ast.encode_string body in
  let result = Api.communicate body in
  match Ast.decode result with
  | Some result ->
    Format.printf "Decoded@.%s@." result
  | None ->
    Format.printf "RAW@.%s@." result
