
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let v = Ast.parse_input body in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr v) in
  ()
