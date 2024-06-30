(* double sin(double x, int n, double s = 1) *)
(* { *)
(*     if(n==1) *)
(*     { *)
(*         return s*x; *)
(*     } *)
(*     else *)
(*     { *)
(*         s = 1 - s*((x*x)/((2*n-1)*(2*n-2))); *)
(*     } *)
(*     return sin(x, n-1, s); *)
(* } *)

#use "topfind";;
#require "zarith";;


let scale = Z.of_int 10_000_000_000

let mul a b =
  Z.div
    (Z.mul a b) scale

let div a b =
  Z.div (Z.mul a scale) b

let ( ** ) = mul
let ( // ) = div

let rec isin x n s =
  if n = 1 then mul s x
  else
    let s =
      Z.sub scale
        (s ** (Z.div (x**x)

                 (

                     (Z.mul

                        (Z.of_int (2*n-1))

                        (Z.of_int (2*n-2)) ))))
    in
    isin x (n-1) s

let rec isin x n s =

  Format.printf "n: %d@.x: %a@.s: %a@.@."
    n Z.pp_print x Z.pp_print s;

  if n = 1 then mul s x
  else
    let f =
                     (Z.mul

                        (Z.of_int (2*n-1))

                        (Z.of_int (2*n-2)))
    in
    let fss =
      (Z.mul (Z.mul scale scale)
               f
      )
    in
    let a2s =
      (Z.mul s (Z.mul x x))
    in

    let p = Z.div a2s fss in
    let s =
      Z.sub scale p
    in
    Format.printf "f: %a@.fss: %a@.a2s: %a@.p: %a@.s_loop: %a@.@."
      Z.pp_print f Z.pp_print fss Z.pp_print a2s Z.pp_print p Z.pp_print s;

    isin x (n-1) s


let visin x =
  let x = int_of_float (x *. 1000_000_000.) in
  let r = isin (Z.of_int x) 15 scale in
  (float @@ Z.to_int r) /. 1000_000_000.


let rec tsin x n s =
  if n = 1 then s *. x
  else
    let s =
      1. -. s *. ((x*.x)/.( float (2*n-1)
                           *. float (2*n-2)))
    in
    tsin x (n-1) s

let vsin x =
  tsin x 10 1.

let diff_sin x =
  abs_float (sin x -. visin x)

let _ =
  let r = isin (Z.of_int 50000000) 10 scale in
  Format.printf "RES: %a@." Z.pp_print r


let p9 = Z.of_string "1_000_000_000"
let p10 = Z.of_string "10_000_000_000"
let p18 = Z.of_string "1_000_000_000_000_000_000"
let rec isin2 a n s =

  Format.printf "n: %d@.x: %a@.s: %a@.@."
    n Z.pp_print x Z.pp_print s;

  if n = 1 then Z.div (Z.mul s a) p9
  else
    let f = (Z.mul (Z.of_int (2*n-1)) (Z.of_int (2*n-2))) in
    let fss = (Z.mul p18 f) in
    let a2s = (Z.mul s (Z.mul a a)) in

    let p = Z.div a2s fss in
    let s = Z.sub p10 p
    in
    Format.printf "f: %a@.fss: %a@.a2s: %a@.p: %a@.s_loop: %a@.@."
      Z.pp_print f Z.pp_print fss Z.pp_print a2s Z.pp_print p Z.pp_print s;

    isin2 a (n-1) s

let visin2 a = isin2 a 15 p10
let () = print_endline (Z.to_string (visin2 (Z.of_string "1047197551")))



(* let l = List.init 160 (fun i -> let x = 0.01 *. float i in diff_sin x) *)

(* let v = visin 0.5 *)
