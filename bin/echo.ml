(* let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n" *)

(* let u n = *)
(*   Printf.sprintf "B%c I! I!" chars.[n] *)

open Miniterm

(* type t = Int | Bool | String (\* | Func *\) *)

(* let ts = [Int; Bool; String] *)
(* let cases = *)
(*   Array.of_list @@ *)
(*   List.flatten @@ *)
(*   List.flatten @@ *)
(*   List.map (fun a -> List.map (fun b -> List.map (fun r -> (a, b, r)) ts) ts) ts *)

(* let test_ret t e = *)
(*   match t with *)
(*   | Int -> Ast.Unop (String_to_int, e) *)
(*   | String -> e *)
(*   | Bool -> if_ e (!~ "a") (!~ "b") *)

(* let test_arg t = *)
(*   match t with *)
(*   | Int -> !+ 1 *)
(*   | Bool -> true_ *)
(*   | String -> !~ "a" *)

(* let pp ppf = function *)
(*   | Int -> Format.fprintf ppf "Int" *)
(*   | Bool -> Format.fprintf ppf "Bool" *)
(*   | String -> Format.fprintf ppf "String" *)

(* let test_case a1 a2 r = *)
(*   Format.printf "%a %a -> %a@." pp a1 pp a2 pp r; *)
(*   let t = !~  "echo " ^/ test_ret r (Binop (Mystery_2, test_arg a1, test_arg a2)) in *)
(*   Format.asprintf "%a" Ast.print_ast t *)

let app a b = Ast.Binop (Apply, a, b)
let op1 a b = Ast.Binop (Apply_strict, a, b)
let op2 a b = Ast.Binop (Apply_mystery, a, b)
(* let term = op2 (op1 (lambda (fun x -> x)) (lambda (fun x -> x))) !~ "plop" *)

let term = op1 (lambda (fun _x -> !~"a")) (!+1 ^/ !+1)

let t s =
  "B. S" ^ Ast.Encoded_string.(to_raw_string (from_string "echo ")) ^ " " ^ s

let f body =
  let body = t body in
  Format.printf "%s@." body;
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  let r = Eval.eval EnvEmpty (Eval.term_from_expr msg) in
  Format.printf "%a@." Eval.pp_value r

(* let test i = *)
(*   Format.printf "test: %i %c@." i chars.[i]; *)
(*   let body = t (u i) in *)
(*   f body; *)
(*   Format.printf "@.@." *)

(* let () = *)
(*   let n = int_of_string Sys.argv.(1) in *)
(*   for i = n to Array.length cases - 1 do *)
(*     Format.printf "@.%i: @." i; *)
(*     let (a, b, r) = cases.(i) in *)
(*     f (test_case a b r); *)
(*     Unix.sleepf 6. *)
(*   done *)

let () =
  let t = !~  "echo " ^/ term in
  let s = Format.asprintf "%a" Ast.print_ast t in
  f s

(* let () = *)
(*   assert(Array.length Sys.argv >= 2); *)
(*   let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in *)
(*   f body *)
