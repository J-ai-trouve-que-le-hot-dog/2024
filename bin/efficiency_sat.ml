
type cnf = (bool * int) list list

let try_2 expr =
  let minvar = ref 10000 in
  let maxvar = ref 0 in
  let exists = Array.make 100 false in
  let get_var v =
    minvar := min !minvar v;
    maxvar := max !maxvar v;
    exists.(v) <- true;
    v
  in
  let rec go_lit = function
    | Ast.Unop(Not,x) -> let (b,e) = go_lit x in (not b, e)
    | Var(x) -> (false, get_var (x+10))
  in
  let rec go_dnf = function
    | Ast.Binop(Or, e1, e2) -> go_dnf e1 @ go_dnf e2
    | e -> [go_lit e]
  in
  let rec go_cnf = function
    | Ast.Binop(And, e1, e2) -> go_cnf e1 @ go_cnf e2
    | e -> [go_dnf e]
      (* Format.printf "%a@." Ast.pp_expr e *)
  in
  let c = go_cnf expr in
  List.iter (fun d ->
      List.iter (fun (b,e) ->
          if b then
            Printf.printf "-";
          Printf.printf "%d " e;
        ) d;
      print_newline ()
    ) c;

  let arr = Array.make 100 false in
  for i = !maxvar downto !minvar do
    let solver = Minisat.create () in
    List.iter (fun d -> Minisat.add_clause_l solver (List.map (fun (b,x) ->
        Minisat.Lit.apply_sign (not b) (Minisat.Lit.make x)) d)) c;
    arr.(i) <- false;
    begin
      try
        for j = !maxvar downto i do
          Minisat.add_clause_l solver [Minisat.Lit.apply_sign arr.(j) (Minisat.Lit.make j)];
        done;
        Minisat.solve solver
      with
      | Minisat.Unsat ->
        arr.(i) <- true
    end
  done;
  let res = ref Z.zero in
  for i = !maxvar downto !minvar do
    res := Z.add (Z.mul !res (Z.of_int 2)) (Z.of_int (if arr.(i) then 1 else 0))
  done;
  Format.printf "Answer: %a@." Z.pp_print !res;
  exit 0

let try_9 expr =
  let minvar = ref 10000 in
  let maxvar = ref 0 in
  let exists = Array.make 100 false in
  let get_var v =
    minvar := min !minvar v;
    maxvar := max !maxvar v;
    exists.(v) <- true;
    v
  in
  let graph = Array.init 100 (fun _ -> CCVector.create ()) in
  let arr = Array.make 100 (-1) in
  let rec go = function
    | Ast.Binop(And, e1, e2) -> go e1; go e2
    | Ast.Unop(Not, Ast.Binop(Eq, Ast.Var(x), Ast.Var(y))) ->
      let x = get_var x in
      let y = get_var y in
      CCVector.push graph.(x) y;
      CCVector.push graph.(y) x;
      ()
    | Ast.Binop(Eq, Ast.Var(x), Ast.Int(v)) ->
      assert(arr.(get_var x) = (-1));
      arr.(get_var x) <- Z.to_int v - 1;
      ()
    | e -> ()
      (* Format.printf "%a@." Ast.pp_expr e *)
  in
  go expr;
  let iters = ref 0 in
  let rec solve depth () =
    incr iters;
    if !iters mod 100000 = 0 then begin
      Printf.printf "iters = %d, depth = %d" !iters depth;
      print_newline ();
      Array.iter (Printf.printf "%d ") arr;
      print_newline ()
    end;
    let bi = ref (-1) in
    let chosen = ref false in
    for i = !maxvar downto !minvar do
      if not !chosen then begin
        if arr.(i) = -1 then begin
          bi := i;
          let set = Array.make 9 1 in
          CCVector.iter (fun j ->
              if arr.(j) <> -1 then
                set.(arr.(j)) <- 0
            ) graph.(i);
          let options = Array.fold_left (+) 0 set in
          if options <= 1 then begin bi := i; chosen := true end
        end
      end
    done;
    if !bi = -1
    then begin
      Format.printf "Solved\n";
      Array.iter (Printf.printf "%d ") arr;
      print_newline ();
      let res = ref Z.zero in
      for i = !minvar to !maxvar do
        if exists.(i) then
          res := Z.add (Z.mul !res (Z.of_int 9)) (Z.of_int arr.(i))
      done;
      Format.printf "Answer: %a@." Z.pp_print !res;
      exit 0
    end
    else begin
      let set = Array.make 9 1 in
      CCVector.iter (fun j ->
          if arr.(j) <> -1 then
            set.(arr.(j)) <- 0
        ) graph.(!bi);
      for d = 0 to 8 do
        if set.(d) = 1 then begin
          arr.(!bi) <- d;
          solve (depth+1) ();
          arr.(!bi) <- (-1)
        end
      done
    end
  in
  solve 0 ()
  
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string body)) in
  let result = Api.communicate body in
  let expr = Ast.parse_input result in
  let exception Found of Ast.expr in
  let rec go : Ast.expr -> unit = function
    | Lambda(b) -> go b.body
    | If(b) -> raise (Found(b.cond))
    | Binop(_,x,y) -> go x; go y
    | Unop(_,x) -> go x
    | Var(_) -> ()
  in
  try go expr
  with Found(cond) ->
    try_2 cond;
    (* try_9 cond; *) (* solves a sudoku *)
    Format.printf "%a@." Ast.pp_expr cond
