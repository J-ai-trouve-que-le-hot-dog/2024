open Ast

let vars = ref 0
let var () =
  incr vars;
  !vars

let (!!) v = Var v
let (!+) i = Int (Z.of_int i)
let (!~) s = String (Encoded_string.from_string s)
let (+/) a b = Binop (Add, a, b)

let (!~+) e = Unop (Int_to_string, e)

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


let pair x y = lambda (fun f -> (f @/ x) @/ y)
let fst p = p @/ (lambda (fun x -> lambda (fun _ -> x)))
let snd p = p @/ (lambda (fun _ -> lambda (fun y -> y)))

let church n =
  lambda (fun f -> lambda (fun z ->
      let rec go = function
        | 0 -> z
        | n -> f @/ (go (n-1))
      in go n
    ))

let one = lambda (fun f -> lambda (fun x -> f @/ x))
let two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x)))
let succ = lambda (fun n -> lambda (fun f -> lambda (fun z ->
    (n @/ f) @/ (f @/ z))))
let add n m = (n @/ succ) @/ m
let mult n m = lambda (fun x -> n @/ (m @/ x))

let lambdaman8 =
  let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in
  let* _4 = two @/ two in
  let* _256 = _4 @/ _4 in
  !~ "solve lambdaman8 " ^/
  (_256 @/ (lambda (fun s ->
      let t1 = (_256 @/ (lambda (fun s -> s ^/ !~ "D"))) @/ s in
      let t2 = (_256 @/ (lambda (fun s -> s ^/ !~ "L"))) @/ t1 in
      let t3 = (_256 @/ (lambda (fun s -> s ^/ !~ "U"))) @/ t2 in
      let t4 = (_256 @/ (lambda (fun s -> s ^/ !~ "R"))) @/ t3 in
      t4
    ))) @/ (!~ "")

let lambdaman9 =
  let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in
  let* _4 = two @/ two in
  let* _256 = _4 @/ _4 in
  !~ "solve lambdaman9 " ^/
  (_256 @/ (lambda (fun s ->
      let t1 = (_256 @/ (lambda (fun s -> s ^/ !~ "R"))) @/ s in
      let t2 = (_256 @/ (lambda (fun s -> s ^/ !~ "L"))) @/ t1 in
      t2 ^/ !~ "D"
    ))) @/ (!~ "")
