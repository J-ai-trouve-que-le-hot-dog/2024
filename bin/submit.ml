
let read_file file = In_channel.with_open_bin file In_channel.input_all
    
let () =
  assert(Array.length Sys.argv == 3);
  let problem = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let file = read_file filename in
  let body = "solve " ^ problem ^ " " ^ file in
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string body)) in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in 
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r
