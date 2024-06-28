

let () =
  let body = In_channel.input_all stdin in
  let v = Ast.parse_input body in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr v) in
  ()
