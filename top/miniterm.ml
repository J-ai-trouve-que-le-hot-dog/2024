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
  Lambda { var = v; body = e (Var v) }

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
  let s = s_repeat 'R' 4 in
  let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in
  !~ "solve lambdaman6 " ^/ conc @/ (conc @/ (conc @/ s))

(* let t = *)
(*   let* s = s_repeat 'R' 6 in *)
(*   let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in *)
(*   !~ "solve lambdaman6 " ^/ conc @/ (conc @/ (conc @/ s)) *)

let ev e =
  let r = Eval.eval EnvEmpty (Eval.term_from_expr e) in
  Format.asprintf "%a" Eval.pp_value r

let v = ev t
let a = Format.asprintf "%a" Ast.print_ast t
let n = String.length a

let u = Ast.parse_input a
let v' = ev u

(* let x = *)
(*   (s_repeat 'R' 4 ^/ s_repeat 'L' 4) *)

(* let x = *)
(*   let* conc = lambda (fun s -> s ^/ s) in *)
(*   let* a = s_repeat 'R' 4 in *)
(*   conc @/ a *)

(* let v = ev x *)
