open Miniterm

let s_repeat c n = !~ (String.init n (fun _ -> c))

let solution_6 =
  let s = s_repeat 'R' 8 in
  let* conc = lambda (fun s -> s ^/ s ^/ s) in
  !~ "solve lambdaman6 " ^/ (conc @/ (conc @/ (conc @/ s)))

let solutions = [
    4, !~ "solve lambdaman4 " ^/ lambdaman4;
    5, !~ "solve lambdaman5 " ^/ lambdaman5;
    6, solution_6;
    7, !~ "solve lambdaman7 " ^/ lambdaman7;
    8, lambdaman8;
    9, lambdaman9;

    10, !~ "solve lambdaman10 " ^/ lambdaman10;

    17, !~ "solve lambdaman17 " ^/ lambdaman17;
    18, !~ "solve lambdaman18 " ^/ lambdaman18;
]

(* module IntMap = Map.Make(Int) *)
(* let solutions = IntMap.of_list solutions *)


let solution_6 = "B$ L1 B. S3/,6%},!-\"$!-!.[} B$ v1 B$ v1 B$ v1 SLLLL L0 B. v0 B. v0 B. v0 v0"
