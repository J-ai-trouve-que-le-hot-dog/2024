
let read_int () =
  let i = ref 0 in
  Scanf.scanf " %d" (fun x -> i := x);
  !i

let () =
  let pb = read_int () in
  let nsteps = read_int () in
  let nseeds = read_int () in
  let seeds = List.init nseeds (fun _ -> read_int ()) in
  let f = if nseeds <= 16 then Miniterm.random_strings else Miniterm.random_strings_64 in
  let e = f nsteps seeds in

  let result = Api.communicate
      (Format.asprintf "%a" Ast.print_ast Miniterm.(!~ ("solve lambdaman" ^ string_of_int pb ^ " ") ^/ e)) in
  let msg = Ast.parse_input result in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r;
  

  ()
