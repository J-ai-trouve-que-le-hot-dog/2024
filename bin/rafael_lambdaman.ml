
let () =
  let VString(t) = Eval.eval Eval.EnvEmpty (Eval.term_from_expr Miniterm.lambdaman8) in
  let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in
  Format.printf "%s\n" s
