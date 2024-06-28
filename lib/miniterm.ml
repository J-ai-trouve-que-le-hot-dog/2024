open Ast

let vars = ref 0
let var () =
  incr vars;
  !vars

let (!!) v = Var v
let (!+) i = Int (Z.of_int i)
let (!~) s = String (Encoded_string.from_string s)
let (+/) a b = Binop (Add, a, b)

let ( ^/ ) a b = Binop (Concat, a, b)

let ( %/ ) a b = Binop (Mod, a, b)
let ( // ) a b = Binop (Div, a, b)

let take s n = Binop (Take, n, s)
let drop s n = Binop (Drop, n, s)

let true_ = Bool true
let false_ = Bool false

let if_ cond tbranch fbranch = If { cond; tbranch; fbranch }

let lambda e =
  let v = var () in
  Lambda { var = v; body = e (Var v) }

let (@/) f x = Binop(Apply, f, x)

let ( let* ) value e =
  match value with
  | Var _ -> e value
  | _ ->
    let v = var () in
    Binop (Apply, Lambda { var = v; body = e (Var v) }, value)

let t encoded_data =
  let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in
  let _4 = two @/ two in
  let _16 = _4 @/ two in
  let many = _16 @/ two in
  (* let many = _16 @/ two in *)
  ((many @/ lambda (fun f -> lambda (fun encoded ->
    take (drop !~ "UDLR" (encoded %/ !+ 4)) !+1 ^/ (f @/ (encoded // !+ 4))
     )))
   @/ (lambda (fun _ -> !~ "")))
  @/ (Int encoded_data)

let to_lambdaman_int s =
  let n = ref Z.zero in
  for i = String.length s - 1 downto 0 do
    let c = s.[i] in
    let c =
      match c with
      | 'U' -> 0
      | 'D' -> 1
      | 'L' -> 2
      | 'R' -> 3
      | _ -> assert false
    in
    n := Z.add (Z.mul (Z.of_int 4) !n) (Z.of_int c)
  done;
  !n

let lambdaman_solution n solution =
  !~ (Printf.sprintf "solve lambdaman%d " n) ^/ t (to_lambdaman_int solution)

let run e =
  let r = Eval.eval EnvEmpty (Eval.term_from_expr e) in
  Format.asprintf "%a" Eval.pp_value r
