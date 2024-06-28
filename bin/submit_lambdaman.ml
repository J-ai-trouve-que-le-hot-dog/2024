open Lamdaman_handcoded

let n = int_of_string (Sys.argv.(1))

let solution = IntMap.find n solutions

let () =
  let body = Format.asprintf "%a" Ast.print_ast solution in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r

