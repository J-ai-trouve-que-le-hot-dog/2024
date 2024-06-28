
let t s =
  "B. S" ^ Ast.Encoded_string.(to_raw_string (from_string "echo ")) ^ " " ^ s

let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = t body in
  Format.printf "%s@." body;
  let result = Api.communicate body in
  let msg = Ast.parse_input result in 
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r
