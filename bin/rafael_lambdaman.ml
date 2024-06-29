
let () =
  (* Random.self_init (); *)
  (* let seed = Random.int 10000000 in *)
  (* let seed = 2346767 in *)
  (* Format.printf "%d@." seed; *)
  (* let e = Miniterm.random_string seed in *)
  (* Format.printf "%a\n" Ast.print_ast Miniterm.(!~ "solve lambdaman7 " ^/ e); *)

  let e = Miniterm.random_strings 62500
      [1]
  in
  let VString(t) = Eval.eval Eval.EnvEmpty (Eval.term_from_expr e) in
  let t = Rope.to_string t in
  let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in
  Format.printf "%s\n" s;

  (* let t = Eval.term_from_expr Miniterm.lambdaman19 in *)
  (* Format.printf "%a@." Ast.pp_expr Miniterm.lambdaman19; *)
  
  (* let result = Api.communicate (Format.asprintf "%a" Ast.print_ast *)
  (*                                 Miniterm.(!~ "solve lambdaman10 " ^/ e)) in *)
  (* let msg = Ast.parse_input result in *)
  (* let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in *)
  (* Format.printf "%a@." Eval.pp_value r; *)
  

  ()
