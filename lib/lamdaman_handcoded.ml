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

    11, !~ "solve lambdaman11 " ^/ lambdaman11;
    12, !~ "solve lambdaman12 " ^/ lambdaman12;
    13, !~ "solve lambdaman13 " ^/ lambdaman13;
    14, !~ "solve lambdaman14 " ^/ lambdaman14;
    15, !~ "solve lambdaman15 " ^/ lambdaman15;

    17, lambdaman17;
    18, lambdaman18;
    21, lambdaman21;
]

(* module IntMap = Map.Make(Int) *)
(* let solutions = IntMap.of_list solutions *)


let solution_6 = "B$ L1 B. S3/,6%},!-\"$!-!.[} B$ v1 B$ v1 B$ v1 SLLLL L0 B. v0 B. v0 B. v0 v0"
