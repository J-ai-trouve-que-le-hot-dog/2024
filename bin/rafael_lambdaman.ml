
let () =
  (* let exception Break in *)
  (* for m = 10 to 10000001 do *)
  (*   Format.printf "%d@." m; *)
  (*   let e = Miniterm.random_string' "" 1 (m-2) 91 m in *)
  (*   let VString(t) = Eval.eval Eval.EnvEmpty (Eval.term_from_expr e) in *)
  (*   let t = Rope.to_string t in *)
  (*   let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in *)
  (*   let game = Run_lambdaman.(decode (read_file "inputs/lambdaman10")) in *)
  (*   if Run_lambdaman.(check game (str_sol s)) *)
  (*   then begin *)
  (*     Format.printf "OK@."; *)
  (*     raise Break *)
  (*   end *)
  (*   else *)
  (*     Format.printf "NOK@." *)
  (* done; *)
  
  let e = Miniterm.lambdaman13 in
  let VString(t) = Eval.eval Eval.EnvEmpty (Eval.term_from_expr e) in
  let t = Rope.to_string t in
  let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in
  (* Format.eprintf "%d@.@." (String.length s); *)
  (* Format.printf "%s\n" s; *)

  (* let game = Run_lambdaman.(decode (read_file "inputs/lambdaman11")) in *)
  (* if Run_lambdaman.(check game (str_sol s)) *)
  (* then *)
  (*   Format.printf "OK@." *)
  (* else *)
  (*   Format.printf "NOK@."; *)


  let m = Format.asprintf "%a" Ast.print_ast e in
  Format.printf "%s\n" m;
  Format.printf "%d\n" (String.length m);
  
  (* let result = Api.communicate (Format.asprintf "%a" Ast.print_ast *)
  (*                                 Miniterm.(!~ "solve lambdaman11 " ^/ e)) in *)
  (* let msg = Ast.parse_input result in *)
  (* let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in *)
  (* Format.printf "%a@." Eval.pp_value r; *)
  

  ()
