open Miniterm

let s_repeat c n = !~ (String.init n (fun _ -> c))

let solution_6 =
  let s = s_repeat 'R' 4 in
  let* conc = lambda (fun s -> s ^/ s ^/ s ^/ s) in
  !~ "solve lambdaman6 " ^/ conc @/ (conc @/ (conc @/ s))

let solutions = [6, solution_6]

module IntMap = Map.Make(Int)
(*let solutions = IntMap.of_list solutions*)


let solution_6 = "B$ L1 B. S3/,6%},!-\"$!-!.[} B$ v1 B$ v1 B$ v1 SLLLL L0 B. v0 B. v0 B. v0 v0"
