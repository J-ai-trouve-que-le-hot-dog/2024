open Ast

let vars = ref 0
let var () =
  incr vars;
  !vars

let (!!) v = Var v
let (!+) i = Int (Z.of_int i)
let (!~) s = String (Encoded_string.from_string s)
let (+/) a b = Binop (Add, a, b)
let ( */) a b = Binop (Mul, a, b)
let (-/) a b = Binop (Sub, a, b)

let (!~+) e = Unop (Int_to_string, e)

let ( ^/ ) a b = Binop (Concat, a, b)
    
let ( =/ ) a b = Binop (Eq, a, b)
let ( </ ) a b = Binop (Lt, a, b)

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

let lambdaman6 =
  let* f = lambda (fun f -> lambda (fun n -> if_ (n =/ !+ 0) (!~ "") ((!~ "R") ^/ ((f @/ f) @/ (n -/ !+ 1))))) in
  (f @/ f) @/ !+ 93

let lambdaman8 =
  let* conc3 = lambda (fun s -> s ^/ (s ^/ s) ^/ s) in
  let* conc81 = lambda (fun s -> conc3 @/ (conc3 @/ (conc3 @/ (conc3 @/ s)))) in
  !~ "solve lambdaman8 " ^/ (conc81 @/ ((conc81 @/ !~ "DL")  ^/ (conc81 @/ !~ "UR")))

(* let lambdaman9 = *)
(*   let* conc3 = lambda (fun s -> s ^/ (s ^/ s) ^/ s) in *)
(*   let* conc81 = lambda (fun s -> conc3 @/ (conc3 @/ (conc3 @/ s))) in *)
(*   !~ "solve lambdaman9 " ^/ (conc81 @/ ((conc81 @/ !~ "R") ^/ (conc81 @/ !~ "L") ^/ !~ "D")) *)

(* let lambdaman16 = *)
(*   let* p1 = lambda (fun i -> (i +/ !+ 1) %/ (!+ 4)) in *)
(*   let* p2 = lambda (fun i -> (i +/ !+ 2) %/ (!+ 4)) in *)
(*   let* p3 = lambda (fun i -> (i +/ !+ 3) %/ (!+ 4)) in *)
(*   let* get2 = lambda (fun i -> take (drop (!~ "URDL") i) (!+ 1)) in *)
(*   (\* let* get2 = lambda (fun i -> (get @/ i) ^/ (get @/ i)) in *\) *)
(*   let* y = lambda (fun f -> *)
(*       let om = lambda (fun x -> f @/ (x @/ x)) in *)
(*       om @/ om *)
(*     ) in *)
(*   let body call = lambda (fun i -> lambda (fun d -> lambda (fun dir -> *)
(*       if_ (i =/ !+ 0) *)
(*         (!~ "") *)
(*         (failwith "TODO") *)
(*     ))) in *)
(*   let* f = (y @/ (lambda body)) @/ !+ 2 in *)
(*   (f @/ !+ 0) @/ !+ 1 *)

let lambdaman19 =
  let* p1 = lambda (fun i -> (i +/ !+ 1) %/ (!+ 4)) in
  let* p2 = lambda (fun i -> (i +/ !+ 2) %/ (!+ 4)) in
  let* p3 = lambda (fun i -> (i +/ !+ 3) %/ (!+ 4)) in
  let* get = lambda (fun i -> take (drop (!~ "URDL") i) (!+ 1)) in
  let* y = lambda (fun f ->
      let om = lambda (fun x -> f @/ (x @/ x)) in
      om @/ om
    ) in
  let pow2_body call = lambda (fun i -> lambda (fun s -> 
      if_ (i =/ !+ 0)
        s
        ((call @/ (i -/ !+ 1)) @/ (s ^/ s))))
  in
  let* pow2 = (y @/ lambda pow2_body) in
  let body call = lambda (fun i -> lambda (fun d ->
      if_ ((i +/ !+ 1) =/ !+ 0)
        (!~ "")
        (((pow2 @/ i) @/ (get @/ d))
         ^/ ((call @/ (i -/ !+ 1)) @/ (p1 @/ d))
         ^/ ((call @/ (i -/ !+ 1)) @/ d)
         ^/ ((call @/ (i -/ !+ 1)) @/ (p3 @/ d))
         ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d)))
        )
    )) in
  let* f = (y @/ (lambda body)) @/ !+ 6 in
  (f @/ !+ 0) ^/
  (f @/ !+ 1) ^/
  (f @/ !+ 2) ^/
  (f @/ !+ 3)

let lambdaman20 =
  let* p1 = lambda (fun i -> (i +/ !+ 1) %/ (!+ 4)) in
  let* p2 = lambda (fun i -> (i +/ !+ 2) %/ (!+ 4)) in
  let* p3 = lambda (fun i -> (i +/ !+ 3) %/ (!+ 4)) in
  let* get = lambda (fun i -> take (drop (!~ "URDL") i) (!+ 1)) in
  let* y = lambda (fun f ->
      let om = lambda (fun x -> f @/ (x @/ x)) in
      om @/ om
    ) in
  let pow2_body call = lambda (fun i -> lambda (fun s -> 
      if_ (i =/ !+ 0)
        s
        ((call @/ (i -/ !+ 1)) @/ (s ^/ s))))
  in
  let* pow2 = (y @/ lambda pow2_body) in
  let body call = lambda (fun i -> lambda (fun d ->
      if_ ((i -/ !+ 0) </ !+ 0)
        (!~ "")
        (((pow2 @/ i) @/ (get @/ d))
         ^/ ((call @/ (i -/ !+ 2)) @/ (p1 @/ d))
         ^/ ((pow2 @/ i) @/ (get @/ d))
         ^/ ((call @/ (i -/ !+ 1)) @/ (p3 @/ d)) ^/ (get @/ (p1 @/ d)) (* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p1 @/ d))) *)
         ^/ ((call @/ (i -/ !+ 1)) @/ d) ^/ (get @/ (p2 @/ d)) (* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p2 @/ d))) *)
         ^/ ((call @/ (i -/ !+ 1)) @/ (p1 @/ d)) ^/ (get @/ (p3 @/ d)) (* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p3 @/ d))) *)
         ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d)))
         ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d)))
         (* ^/ (get @/ (p2 @/ d)) *)
        )
    )) in
  let* f = (y @/ (lambda body)) @/ !+ 5 in
  (f @/ !+ 0)(* ^/ (get @/ !+ 2) ^/
  (f @/ !+ 1) ^/ (get @/ !+ 3) ^/
  (f @/ !+ 2) ^/ (get @/ !+ 0) ^/
  (f @/ !+ 3) *)

let lambdaman21 =
  let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in
  let* _4 = two @/ two in
  let* _8 = mult _4 two in
  let* _16 = _4 @/ two in
  let* _64 = mult _16 _4 in
  let* _128 = mult _64 two in
  let* _256 = _4 @/ _4 in
  let* s_l =
    (_256 @/ (lambda (fun s ->
         let t1 = (_256 @/ (lambda (fun s -> s ^/ !~ "D"))) @/ s in
         let t2 = (_256 @/ (lambda (fun s -> s ^/ !~ "U"))) @/ t1 in
         t2 ^/ !~ "DL"
       ))) @/ (!~ "")
  in
  let* s_r =
    (_256 @/ (lambda (fun s ->
         let t1 = (_256 @/ (lambda (fun s -> s ^/ !~ "D"))) @/ s in
         let t2 = (_256 @/ (lambda (fun s -> s ^/ !~ "U"))) @/ t1 in
         t2 ^/ !~ "DR"
       ))) @/ (!~ "")
  in
  let* s_r2 =
    (_256 @/ (lambda (fun s ->
         let t1 = (_256 @/ (lambda (fun s -> s ^/ !~ "U"))) @/ s in
         let t2 = (_256 @/ (lambda (fun s -> s ^/ !~ "D"))) @/ t1 in
         t2 ^/ !~ "UR"
       ))) @/ (!~ "")
  in
  let* l256 =
    (_256
     @/ (lambda (fun s -> s ^/ !~ "L")))
    @/ (!~ "")
  in
  let* r256 =
    (_256
     @/ (lambda (fun s -> s ^/ !~ "R")))
    @/ (!~ "")
  in
  let* d64 =
    (_64
     @/ (lambda (fun s -> s ^/ !~ "D")))
    @/ (!~ "")
  in
  let* d8 =
    (_8
     @/ (lambda (fun s -> s ^/ !~ "D")))
    @/ (!~ "")
  in
  let* u64 =
    (_64
     @/ (lambda (fun s -> s ^/ !~ "U")))
    @/ (!~ "")
  in
  let* u8 =
    (_8
     @/ (lambda (fun s -> s ^/ !~ "U")))
    @/ (!~ "")
  in
  d8 ^/ d8 ^/ d8 ^/ !~ "U" ^/ l256
  ^/ s_r
  ^/ s_l ^/ d64 ^/ d8 ^/ r256 ^/ s_l ^/ d64 ^/ d64 ^/ u8 ^/ r256 ^/ s_l ^/ s_r2
  ^/ u64 ^/ u64 ^/ d8 ^/ d8 ^/ d8 ^/ !~ "DLLLLLLLLLLLLLLLLLLLLLLLLLL" ^/ s_l

(* let random_string seed = *)
(*   let () = vars := 0 in *)
(*   let y = lambda (fun f -> *)
(*       let* om = lambda (fun x -> f @/ (x @/ x)) in *)
(*       om @/ om *)
(*     ) in *)
(*   let get i = take (drop (!~ "URDL") i) (!+ 1) in *)
(*   let body call = lambda (fun i -> (lambda (fun s -> *)
(*       if_ (i =/ !+ 0) *)
(*         (!~ "") *)
(*         ( ((call @/ (i -/ !+ 1)) @/ (((s */ s) +/ s) %/ !+ 1000000009)) ^/ *)
(*           (get (s %/ !+ 4)) *)
(*         ) *)
(*     ))) *)
(*   in *)
(*   ((y @/ lambda body) @/ !+ 1000000) @/ !+ seed *)

(* let compute_params l = *)
(*   let mx = List.fold_left max 0 l in *)
(*   let l = List.rev l in *)
(*   mx+1, List.fold_left (fun acc x -> Z.add (Z.mul (Z.of_int (mx + 1)) acc) (Z.of_int x)) Z.zero l *)

(* let random_strings nsteps maxseed encoded_data = *)
(*   let () = vars := 0 in *)
(*   let y f = *)
(*       let* om = lambda (fun x -> f (x @/ x)) in *)
(*       om @/ om *)
(*   in *)
(*   let get i = take (drop (!~ "UDLR") i) (!+ 1) in *)
(*   let body call = lambda (fun i -> (lambda (fun s -> *)
(*       if_ (i =/ !+ 0) *)
(*         (!~ "") *)
(*         ( (get (s %/ !+ 4)) ^/ *)
(*           ((call @/ (i -/ !+ 1)) @/ ((s */ !+ 48271) %/ !+ 2147483647))  *)
(*         ) *)
(*     ))) *)
(*   in *)
(*   let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in *)
(*   let _4 = two @/ two in *)
(*   let _16 = _4 @/ two in *)
(*   ((_16 @/ lambda (fun f -> lambda (fun encoded -> *)
(*     (((y body) @/ !+ nsteps) @/ (encoded %/ !+ maxseed)) *)
(*     ^/ (f @/ (encoded // !+ maxseed)) *)
(*      ))) *)
(*    @/ (lambda (fun _ -> !~ ""))) *)
(*   @/ (Int encoded_data) *)

(* let random_strings nsteps data = *)
(*   let mx, encoded = compute_params data in *)
(*   random_strings nsteps mx encoded *)


(* let random_strings_64 nsteps maxseed encoded_data = *)
(*   let () = vars := 0 in *)
(*   let y f = *)
(*       let* om = lambda (fun x -> f (x @/ x)) in *)
(*       om @/ om *)
(*   in *)
(*   let get i = take (drop (!~ "UDLR") i) (!+ 1) in *)
(*   let body call = lambda (fun i -> (lambda (fun s -> *)
(*       if_ (i =/ !+ 0) *)
(*         (!~ "") *)
(*         ( (get (s %/ !+ 4)) ^/ *)
(*           ((call @/ (i -/ !+ 1)) @/ ((s */ !+ 48271) %/ !+ 2147483647))  *)
(*         ) *)
(*     ))) *)
(*   in *)
(*   let* two = lambda (fun f -> lambda (fun x -> f @/ (f @/ x))) in *)
(*   let* _4 = two @/ two in *)
(*   let _16 = _4 @/ two in *)
(*   let _64 = mult _16 _4 in *)
(*   ((_64 @/ lambda (fun f -> lambda (fun encoded -> *)
(*     (((y body) @/ !+ nsteps) @/ (encoded %/ !+ maxseed)) *)
(*     ^/ (f @/ (encoded // !+ maxseed)) *)
(*      ))) *)
(*    @/ (lambda (fun _ -> !~ ""))) *)
(*   @/ (Int encoded_data) *)

(* let random_strings_64 nsteps data = *)
(*   let mx, encoded = compute_params data in *)
(*   random_strings_64 nsteps mx encoded *)

let random_string' name seed stop c m =
  let () = vars := 0 in
  let y f =
      let* om = lambda (fun x -> f (x @/ x)) in
      om @/ om
  in
  let get i = take (drop (!~ "UDLR") i) (!+ 1) in
  let body call = lambda (fun s ->
      if_ (s =/ !+ stop)
        (!~ name)
        ( (call @/ ((s */ !+ c) %/ !+ m)) ^/
          (get (s %/ !+ 4))
        )
    )
  in
  (y body) @/ !+ seed

let lambdaman4 =
  random_string' "solve lambdaman4 " 10 58 91 12553

let lambdaman5 =
  random_string' "solve lambdaman5 " 5 17 92 1873

let lambdaman7 =
  random_string' "solve lambdaman7 " 89 19 46 6899

let lambdaman9 =
  random_string' "solve lambdaman9 " 49 81 91 33641

let lambdaman10 =
  random_string' "solve lambdaman10 " 30 9 91 42557

let lambdaman17 =
  random_string' "solve lambdaman17 " 37 13 91 200671

let lambdaman21 =
  random_string' "solve lambdaman21 " 5 91 3 999773

let random_string'' name seed stop c m =
 let () = vars := 0 in
 let y f =
   let* om = lambda (fun x -> f (x @/ x)) in
   om @/ om
 in
 let get i =
   let* c = take (drop (!~ "UDLRUDLR") i) (!+ 1) in
   if_ (i </ !+ 4) c (c ^/ c ^/ c)
 in
 let body call = lambda (fun s ->
     if_ (s =/ !+ stop)
       (!~ name)
       ( (call  @/ ((s */ !+ c) %/ !+ m)) ^/
         (get (s %/ !+ 8))
       )
   )
 in
 (y body) @/ !+ seed

let lambdaman18 =
  random_string'' "solve lambdaman18 " 69 56 91 500153

let random_strings_c name c m seed1 stop1 seed2 stop2 =
 let () = vars := 0 in
 let y = (fun f ->
     let* om = lambda (fun x -> f (x @/ x)) in
     om @/ om
   ) in
 let get i = take (drop (!~ "UUDDLLRR") i) (!+ 2) in
 let body call = lambda (fun s ->
     if_ (s =/ !+ (2 * stop2))
       (call @/ !+ (2 * seed1))
       (if_ (s =/ !+ (2 * stop1))
          (!~ name)
          ( (call @/ ((s */ !+ c) %/ !+ (2*m))) ^/
            (get (s %/ !+ 8))
          )
       )
   )
 in
 (* !~ ("solve " ^ name ^ " ") ^/ *)
 ((y body) @/ !+ (2*seed2))

let lambdaman11
  = random_strings_c "solve lambdaman11 " 18 7805419 21 886 12 3363

let lambdaman12
  = random_strings_c "solve lambdaman12 " 4 39037403 37 4235 10 3454
    
let lambdaman13
  = random_strings_c "solve lambdaman13 " 27 39036989 37 976 2 4259
    
let lambdaman14
  = random_strings_c "solve lambdaman14 " 19 39026077 3 3557 40 582

let lambdaman15
  = random_strings_c "solve lambdaman15 " 7 39034117 1 1917 33 4131

let random_strings_c seed1 seed2 =
  let () = vars := 0 in
  let y = (fun f ->
      let* om = lambda (fun x -> f (x @/ x)) in
      om @/ om
    ) in
  let get i = take (drop (!~ "URDL") (i )) (!+ 1) in
  let body call = lambda (fun i -> (lambda (fun s ->
      if_ (i =/ !+ 0)
        (!~ "")
        ( ((call @/ (i -/ !+ 1)) @/ ((s */ !+ 11) %/ !+ 78074891)) ^/
          (get (s %/ !+ 4))
        )
    )))
  in
  let* ff = (y body) @/ !+ 500000 in
  (ff @/ !+ seed1) ^/ (ff @/ !+ seed2)


let random_pow name base expo =
  let () = vars := 0 in
  let y = (fun f -> let* om = lambda (fun x -> f (x @/ x)) in om @/ om) in
  let get i = take (drop (!~ "URDL") i) (!+ 1) in
  let step call = lambda (fun n -> if_ (n =/ !+ 0) (!~ ("solve " ^ name ^ " ")) (
     (call @/ (n // !+ 4)) ^/ get (n %/ !+ 4)
  ))
  in
  let exp call = lambda (fun n -> if_ (n =/ !+ 0) (!+ 1) ((!+ base) */ (call @/ (n -/ !+ 1)))) in
  (y step) @/ ((y exp) @/ (!+ expo))

