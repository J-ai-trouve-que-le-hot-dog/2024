
let filename = Sys.argv.(1)
let offs = int_of_string Sys.argv.(2)

let () =
  (* Random.self_init (); *)
  (* let seed = Random.int 10000000 in *)
  (* let seed = 2346767 in *)
  (* Format.printf "%d@." seed; *)
  (* let e = Miniterm.random_string seed in *)
  (* Format.printf "%a\n" Ast.print_ast Miniterm.(!~ "solve lambdaman7 " ^/ e); *)

  for seed = 1 + offs to 1000 + offs do

  let e = Miniterm.random_string'' seed 490_000 91 in
  let term_s = Format.asprintf "%a" Ast.print_ast Miniterm.(!~ "solve lambdaman18 " ^/ e) in
  let len = String.length term_s in
  (* Format.eprintf "TERM@.%s@.@." term_s; *)
  Format.eprintf "SIZE: %d@.@." len;
  match Eval.eval Eval.EnvEmpty (Eval.term_from_expr e) with
  | VBool _ | VInt _ | VLambda _ -> assert false
  | VString(t) ->
     let t = Rope.to_string t in
     let s = Ast.Encoded_string.to_string (Ast.Encoded_string.from_raw_string t) in
     Format.eprintf "%d@.@." (String.length s);
     let game = Run_lambdaman.run filename s in

     Format.printf "SEED: %i@." seed;
     Format.printf "Missing: %d@.@." game.Run_lambdaman.npills;
     (* Format.printf "WIN ? %b@." (Run_lambdaman.is_win game); *)
     if (Run_lambdaman.is_win game) then
       exit 0;
     ()

  done
  (* let t = Eval.term_from_expr Miniterm.lambdaman19 in *)
  (* Format.printf "%a@." Ast.pp_expr Miniterm.lambdaman19; *)
  
  (* let result = Api.communicate (Format.asprintf "%a" Ast.print_ast *)
  (*                                 Miniterm.(!~ "solve lambdaman10 " ^/ e)) in *)
  (* let msg = Ast.parse_input result in *)
  (* let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in *)
  (* Format.printf "%a@." Eval.pp_value r; *)
