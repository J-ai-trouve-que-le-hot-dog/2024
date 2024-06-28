#use "../top/top.ml";;

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

let lambda e =
  let v = var () in
  Lambda { var = v; body = (Var v) }

let (@/) f x = Binop(Apply, f, x)

let ( let* ) value e =
  let v = var () in
  Binop (Apply, Lambda { var = v; body = e (Var v) }, value)

let u = !~ "U"
let d = !~ "D"
let l = !~ "L"
let r = !~ "R"

let t =
  let* a = !+1 +/ !+ 2 in
  let* b = a +/ !+ 3 in
  !~ "plop"

let s_repeat c n = !~ (String.init n (fun _ -> c))

let t =
  let* s = s_repeat 'R' 4 in
  let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in
  conc @/ (conc @/ (conc @/ s))

let ev e =
  let r = Eval.eval EnvEmpty (Eval.term_from_expr e) in
  Format.asprintf "%a" Eval.pp_value r

let v = ev t
