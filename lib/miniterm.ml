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
  let () = vars := 0 in
  let y f =
      let* om = lambda (fun x -> f (x @/ x)) in
      om @/ om
  in
  let get i = take (drop (!~ "DLUR") i) (!+ 1) in
  let body call = lambda (fun s ->
      if_ (s =/ !+ 0)
        (!~ "solve lambdaman8 ")
        ( (call @/ (s -/ !+ 1)) ^/
          (get ((s // !+ 98) %/ !+ 4))
        )
    )
  in
  (y body) @/ !+ 10000


let lambdaman9 =
  let () = vars := 0 in
  let y f =
      let* om = lambda (fun x -> f (x @/ x)) in
      om @/ om
  in
  let get i = take (drop (!~ "RLD") i) (!+ 1) in
  let body call = lambda (fun s ->
      if_ (s =/ !+ 0)
        (!~ "solve lambdaman9 ")
        ( (call @/ (s -/ !+ 1)) ^/
          (get ((s %/ !+ 101) // !+ 50))
        )
    )
  in
  (y body) @/ !+ 5000



(* let lambdaman9 = *)
(*   let () = vars := 0 in *)
(*   let y f = *)
(*       let* om = lambda (fun x -> f (x @/ x)) in *)
(*       om @/ om *)
(*   in *)
(*   let get i = take (drop (!~ "RLD") i) (!+ 1) in *)
(*   let body call = lambda (fun s -> *)
(*       if_ (s =/ !+ 0) *)
(*         (!~ "solve lambdaman9 ") *)
(*         ( (call @/ (s -/ !+ 1)) ^/ *)
(*           (get ((s %/ !+ 101) // !+ 50)) *)
(*         ) *)
(*     ) *)
(*   in *)
(*   (y body) @/ !+ 5000 *)


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

(* let lambdaman19 = *)
(*   let* p1 = lambda (fun i -> (i +/ !+ 1) %/ (!+ 4)) in *)
(*   let* p2 = lambda (fun i -> (i +/ !+ 2) %/ (!+ 4)) in *)
(*   let* p3 = lambda (fun i -> (i +/ !+ 3) %/ (!+ 4)) in *)
(*   let* get = lambda (fun i -> take (drop (!~ "URDL") i) (!+ 1)) in *)
(*   let* y = lambda (fun f -> *)
(*       let om = lambda (fun x -> f @/ (x @/ x)) in *)
(*       om @/ om *)
(*     ) in *)
(*   let pow2_body call = lambda (fun i -> lambda (fun s ->  *)
(*       if_ (i =/ !+ 0) *)
(*         s *)
(*         ((call @/ (i -/ !+ 1)) @/ (s ^/ s)))) *)
(*   in *)
(*   let* pow2 = (y @/ lambda pow2_body) in *)
(*   let body call = lambda (fun i -> lambda (fun d -> *)
(*       if_ ((i +/ !+ 1) =/ !+ 0) *)
(*         (!~ "") *)
(*         (((pow2 @/ i) @/ (get @/ d)) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ (p1 @/ d)) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ d) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ (p3 @/ d)) *)
(*          ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d))) *)
(*         ) *)
(*     )) in *)
(*   let* f = (y @/ (lambda body)) @/ !+ 6 in *)
(*   (f @/ !+ 0) ^/ *)
(*   (f @/ !+ 1) ^/ *)
(*   (f @/ !+ 2) ^/ *)
(*   (f @/ !+ 3) *)

(* let lambdaman20 = *)
(*   let* p1 = lambda (fun i -> (i +/ !+ 1) %/ (!+ 4)) in *)
(*   let* p2 = lambda (fun i -> (i +/ !+ 2) %/ (!+ 4)) in *)
(*   let* p3 = lambda (fun i -> (i +/ !+ 3) %/ (!+ 4)) in *)
(*   let* get = lambda (fun i -> take (drop (!~ "URDL") i) (!+ 1)) in *)
(*   let* y = lambda (fun f -> *)
(*       let om = lambda (fun x -> f @/ (x @/ x)) in *)
(*       om @/ om *)
(*     ) in *)
(*   let pow2_body call = lambda (fun i -> lambda (fun s ->  *)
(*       if_ (i =/ !+ 0) *)
(*         s *)
(*         ((call @/ (i -/ !+ 1)) @/ (s ^/ s)))) *)
(*   in *)
(*   let* pow2 = (y @/ lambda pow2_body) in *)
(*   let body call = lambda (fun i -> lambda (fun d -> *)
(*       if_ ((i -/ !+ 0) </ !+ 0) *)
(*         (!~ "") *)
(*         (((pow2 @/ i) @/ (get @/ d)) *)
(*          ^/ ((call @/ (i -/ !+ 2)) @/ (p1 @/ d)) *)
(*          ^/ ((pow2 @/ i) @/ (get @/ d)) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ (p3 @/ d)) ^/ (get @/ (p1 @/ d)) (\* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p1 @/ d))) *\) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ d) ^/ (get @/ (p2 @/ d)) (\* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p2 @/ d))) *\) *)
(*          ^/ ((call @/ (i -/ !+ 1)) @/ (p1 @/ d)) ^/ (get @/ (p3 @/ d)) (\* ^/ (if_ (i </ !+ 1) (!~ "") (get @/ (p3 @/ d))) *\) *)
(*          ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d))) *)
(*          ^/ ((pow2 @/ i) @/ (get @/ (p2 @/ d))) *)
(*          (\* ^/ (get @/ (p2 @/ d)) *\) *)
(*         ) *)
(*     )) in *)
(*   let* f = (y @/ (lambda body)) @/ !+ 5 in *)
(*   (f @/ !+ 0)(\* ^/ (get @/ !+ 2) ^/ *)
(*   (f @/ !+ 1) ^/ (get @/ !+ 3) ^/ *)
(*   (f @/ !+ 2) ^/ (get @/ !+ 0) ^/ *)
(*   (f @/ !+ 3) *\) *)

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

let compute_params l =
  let mx = List.fold_left max 0 l in
  let l = List.rev l in
  mx+1, List.fold_left (fun acc x -> Z.add (Z.mul (Z.of_int (mx + 1)) acc) (Z.of_int x)) Z.zero l

let random_strings name m c nsteps maxseed encoded_data =
  let () = vars := 0 in
  let y f =
    let* om = lambda (fun x -> f (x @/ x)) in
    om @/ om
  in
  let get i =
    take (drop (!~ "UDLR") i) (!+ 1)
  in
  let body call = lambda (fun i -> (lambda (fun s ->
      if_ (i =/ !+ 0)
        (!~ "")
        ( (get (s %/ !+ 4)) ^/
          ((call @/ (i -/ !+ 1)) @/ ((s */ !+ c) %/ !+ m))
        )
    )))
  in
  let f1 = y body in
  let body call = lambda (fun encoded ->
      if_ (encoded =/ !+ 0)
        (!~ name)
        ((call @/ (encoded // !+ maxseed)) ^/
         ((f1 @/ !+ nsteps) @/ (encoded %/ !+ maxseed)))
    )
  in
  (y body) @/ (Int encoded_data)

let random_strings name m c nsteps data =
  let mx, encoded = compute_params data in
  random_strings name m c nsteps mx encoded

let random_strings_bias name m c nsteps maxseed encoded_data =
  let () = vars := 0 in
  let y f =
    let* om = lambda (fun x -> f (x @/ x)) in
    om @/ om
  in
  let get i =
    let* c = take (drop (!~ "UDLRUDLR") i) (!+ 1) in
    if_ (i </ !+ 4) (c ^/ c ^/ c) c
  in
  let body call = lambda (fun i -> (lambda (fun s ->
      if_ (i =/ !+ 0)
        (!~ "")
        ( (get (s %/ !+ 8)) ^/
          ((call @/ (i -/ !+ 1)) @/ ((s */ !+ c) %/ !+ m))
        )
    )))
  in
  let f1 = y body in
  let body call = lambda (fun encoded ->
      if_ (encoded =/ !+ 0)
        (!~ name)
        ((call @/ (encoded // !+ maxseed)) ^/
         ((f1 @/ !+ nsteps) @/ (encoded %/ !+ maxseed)))
    )
  in
  (y body) @/ (Int encoded_data)

let random_strings_bias name m c nsteps data =
  let mx, encoded = compute_params data in
  random_strings_bias name m c nsteps mx encoded

let lambdaman16 =
  random_strings "solve lambdaman16 " 3903689 3
    66666
    (List.rev [ 56 ; 48 ; 54 ; 48 ; 23 ; 56 ; 50 ; 16 ; 56 ; 56 ; 28 ; 47 ; 56 ; 26 ; 9 ])


let lambdaman19 = random_strings "solve lambdaman19 " 3903689 3
    34482
    (List.rev [
        91 ; 38 ; 85 ; 49 ; 47 ; 98 ; 47 ; 39 ; 99 ; 22 ; 61 ; 50 ; 49 ; 18 ; 28 ; 75 ; 100 ; 24 ; 71 ; 66 ; 96 ; 51 ; 100 ; 58 ; 80 ; 6 ; 55 ; 25
      ])

let lambdaman20 = random_strings "solve lambdaman20 " 3903689 3 333205
    (List.rev [ 54 ; 37 ; 32
              ])


let random_string' perm name seed stop c m =
  let () = vars := 0 in
  let y f =
      let* om = lambda (fun x -> f (x @/ x)) in
      om @/ om
  in
  let get i = take (drop (!~ perm) i) (!+ 1) in
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
  random_string' "DRLU" "solve lambdaman4 " 64 50 13 8821

let lambdaman5 =
  random_string' "UDLR" "solve lambdaman5 " 5 17 92 1873

let lambdaman7 =
  random_string' "UDLR" "solve lambdaman7 " 89 19 46 6899

(* let lambdaman9 = *)
(*   random_string' "UDLR" "solve lambdaman9 " 49 81 91 33641 *)

let lambdaman10 =
  random_string' "UDLR" "solve lambdaman10 " 30 9 91 42557

let lambdaman17 =
  random_string' "UDLR" "solve lambdaman17 " 37 13 91 200671

let lambdaman18 =
  random_string' "UDLR" "solve lambdaman18 " 9 3 3 812381

let lambdaman21 =
  random_string' "UDLR" "solve lambdaman21 " 42 14 3 830237

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

let random_strings_18 name c m seed1 stop1 seed2 stop2 =
 let () = vars := 0 in
 let y = (fun f ->
     let* om = lambda (fun x -> f (x @/ x)) in
     om @/ om
   ) in
 let get i = take (drop (!~ "UDLR") i) (!+ 1) in
 let body call = lambda (fun s ->
     let next_seed = if_ (s =/ !+ stop2) (!+ seed1) ((s */ !+ c) %/ !+ m) in
     (if_ (s =/ !+ stop1)
        (!~ name)
        ( (call @/ next_seed) ^/
          (get (s %/ !+ 4))
        )
     )
   )
 in
 ((y body) @/ !+ seed2)

(* let lambdaman18 = *)
(*   random_strings_18 "solve lambdaman18 " 3 3903689 4 58 16 44 *)

let random_strings_20 name c m seed1 stop1 seed2 stop2 seed3 stop3 =
 let () = vars := 0 in
 let y = (fun f ->
     let* om = lambda (fun x -> f (x @/ x)) in
     om @/ om
   ) in
 let get i = take (drop (!~ "UDLR") i) (!+ 1) in
 let body call = lambda (fun s ->
     let next_seed =
       if_ (s =/ !+ stop2) (!+ seed1)
         (if_ (s =/ !+ stop3) (!+ seed2)
            ((s */ !+ c) %/ !+ m)) in
     (if_ (s =/ !+ stop1)
        (!~ name)
        ( (call @/ next_seed) ^/
          (get (s %/ !+ 4))
        )
     )
   )
 in
 ((y body) @/ !+ seed3)

let lambdaman20 =
  let m = 3903689 in
  let c = 3 in
  let next x = (x*c) mod m in
  let rec nextn x n = if n = 0 then x else nextn (next x) (n-1) in
  let stop x = nextn x 333205 in
  let seed1 = 1 in
  let seed2 = 25 in
  let seed3 = 28 in
  random_strings_20 "solve lambdaman20 " c m
    seed1 (stop seed1)
    seed2 (stop seed2)
    seed3 (stop seed3)

let random_strings_c name c m seed1 stop1 seed2 stop2 =
 let () = vars := 0 in
 let y = (fun f ->
     let* om = lambda (fun x -> f (x @/ x)) in
     om @/ om
   ) in
 let get i = take (drop (!~ "UUDDLLRR") i) (!+ 2) in
 let body call = lambda (fun s ->
     let next_seed = if_ (s =/ !+ (2*stop2)) (!+ (2*seed1)) ((s */ !+ c) %/ !+ (2*m)) in
     (if_ (s =/ !+ (2*stop1))
        (!~ name)
        ( (call @/ next_seed) ^/
          (get (s %/ !+ 8))
        )
     )
   )
 in
 (* !~ ("solve " ^ name ^ " ") ^/ *)
 ((y body) @/ !+ (2*seed2))

let lambdaman11
  = random_strings_c "solve lambdaman11 " 47 5128213 3 12 7 28

let lambdaman12
  = random_strings_c "solve lambdaman12 " 7 3847469 10 43 34 41
    
let lambdaman13
  = random_strings_c "solve lambdaman13 " 14 3857911 25 31 41 23
    
let lambdaman14
  = random_strings_c "solve lambdaman14 " 28 3854449 40 41 5 13

let lambdaman15
  = random_strings_c "solve lambdaman15 " 15 3765367 44 19 40 45


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

