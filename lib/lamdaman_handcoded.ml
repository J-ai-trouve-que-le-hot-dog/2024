open Miniterm

let s_repeat c n = !~ (String.init n (fun _ -> c))

let solution_6 =
  let s = s_repeat 'R' 8 in
  let* conc = lambda (fun s -> s ^/ s ^/ s) in
  !~ "solve lambdaman6 " ^/ (conc @/ (conc @/ (conc @/ s)))

let solutions = [
    4, lambdaman4;
    5, lambdaman5;
    6, solution_6;
    7, lambdaman7;
    8, lambdaman8;
    9, lambdaman9;

    10, lambdaman10;

    11, lambdaman11;
    12, lambdaman12;
    13, lambdaman13;
    14, lambdaman14;
    15, lambdaman15;
    16, lambdaman16;
    17, lambdaman17;
    18, lambdaman18;
    19, lambdaman19;
    20, lambdaman20;
    21, lambdaman21;
]

(* module IntMap = Map.Make(Int) *)
(* let solutions = IntMap.of_list solutions *)


let solution_6 = "B$ L1 B. S3/,6%},!-\"$!-!.[} B$ v1 B$ v1 B$ v1 SLLLL L0 B. v0 B. v0 B. v0 v0"
