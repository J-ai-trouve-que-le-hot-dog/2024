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

let ( %/ ) a b = Binop (Mod, a, b)
let ( // ) a b = Binop (Div, a, b)

let take s n = Binop (Take, n, s)
let drop s n = Binop (Drop, n, s)

let lambda e =
  let v = var () in
  Lambda { var = v; body = e (Var v) }

let (@/) f x = Binop(Apply, f, x)

let ( let* ) value e =
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

(*
let t =
  let s = s_repeat 'R' 4 in
  let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in
  !~ "solve lambdaman6 " ^/ conc @/ (conc @/ (conc @/ s))
*)

(* let t = *)
(*   let* s = s_repeat 'R' 6 in *)
(*   let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in *)
(*   !~ "solve lambdaman6 " ^/ conc @/ (conc @/ (conc @/ s)) *)

let ev e =
  let r = Eval.eval EnvEmpty (Eval.term_from_expr e) in
  Format.asprintf "%a" Eval.pp_value r

let tt = (t (Z.of_int 123456))
let v = ev tt

let a = Format.asprintf "%a" Ast.print_ast tt
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


let v = lambda (fun _ -> !~ "Ah!")
let s = Format.asprintf "%a" Ast.print_ast v

let op x y =
  Binop (Mystery_1, x, y)

let t =
  op (lambda (fun _ -> !~ "Ah!")) !+ 3
