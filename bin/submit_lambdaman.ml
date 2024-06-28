
let solution_6 = "B$ L1 B. S3/,6%},!-\"$!-!.[} B$ v1 B$ v1 B$ v1 SLLLL L0 B. v0 B. v0 B. v0 v0"

let () =
  let body = v in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r

