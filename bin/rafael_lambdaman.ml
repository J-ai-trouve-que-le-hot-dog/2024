
let () =
  (* Random.self_init (); *)
  (* let seed = Random.int 10000000 in *)
  (* let seed = 2346767 in *)
  (* Format.printf "%d@." seed; *)
  (* let e = Miniterm.random_string seed in *)
  (* Format.printf "%a\n" Ast.print_ast Miniterm.(!~ "solve lambdaman7 " ^/ e); *)

  let e = Miniterm.random_strings_64 (62500 / 4)
      [62067;24416;58792;2375;41063;47769;10973;2462;41609;3311;22077;20423;55219;16300;55941;13479;8549;64286;14079;42085;55513;11992;13394;50445;57393;54630;57206;28426;50110;41559;64016;49207;4282;35689;14584;42863;62664;43572;62150;2309;35711;44645;61119;3203]
  in
  let VString(t) = Eval.eval Eval.EnvEmpty (Eval.term_from_expr e) in
  let t = Rope.to_string t in
  let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in
  Format.printf "%s\n" s;

  (* let t = Eval.term_from_expr Miniterm.lambdaman19 in *)
  (* Format.printf "%a@." Ast.pp_expr Miniterm.lambdaman19; *)
  
  let result = Api.communicate (Format.asprintf "%a" Ast.print_ast
                                  Miniterm.(!~ "solve lambdaman20 " ^/ e)) in
  let msg = Ast.parse_input result in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r;
  

  ()
