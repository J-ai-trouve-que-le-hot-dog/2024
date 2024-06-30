open Stdlib
open Comp.Compile

let init_program = { act = []; copies = []; outputs = []; to_init_from  = [] }
let program = ref init_program
let count = ref 0
let reset () =
  program := init_program;
  count := 0
let v ?(i = "") s = Var (V s, i)
let c s = Const s

let init_from ~init ~from =
  program := { !program with to_init_from = { var_to_init = V init; init_from = V from } :: !program.to_init_from }

let add_act res a op b =
  program :=
    { !program with act = { op; l = a; u = b; out = V res } :: !program.act }

let add_copy inv ?(i = "") out1 out2 =
  program :=
    {
      !program with
      copies = { outs = [V out1; V out2]; copied = V inv, i } :: !program.copies;
    }

let add_copy_3 inv ?(i = "") out1 out2 out3 =
  program :=
    {
      !program with
      copies =
        { outs = [V out1; V out2; V out3]; copied = V inv, i }
        :: !program.copies;
    }

let add_out out =
  program := { !program with outputs = V out :: !program.outputs }

let equal_widet =
  let var () =
    incr count;
    Printf.sprintf "Tm%d" !count
  in
  fun out in1 in2 ->
    let v0 = var () in
    add_act v0 in1 '-' in2;
    let v1 = var () in
    let v2 = var () in
    let v3 = var () in
    add_copy_3 v0 v1 v2 v3;

    add_act out (v v1) '=' (c "0");
    add_act out (v v2) '/' (v v3);
    ()

let delay_widget =
  let var () =
    incr count;
    Printf.sprintf "Dm%d" !count
  in
  let rec loop out in_ n =
    assert(n > 0);
    if n = 1 then
      add_act out in_ '+' (c "0")
    else
      let var = var () in
      add_act var in_ '+' (c "0");
      loop out (v var) (n-1)
  in
  loop

let vdelay in_ n =
  let var () =
    incr count;
    Printf.sprintf "Vm%d" !count
  in
  if n = 0 then (v in_)
  else
    let var = var () in
    delay_widget var (v in_) n;
    v var

let a = add_act
let p = add_copy
let p3 = add_copy_3

module M9 () = struct

  let () =

    (* T1 *)

    p3 "U" ~i:"A" "U_1" "U_2" "U_3";
    p3 "Counter" ~i:"-1" "Counter_1" "Counter_2" "Counter_3";

    (* T2 *)
    a "Top" (v "U_1") '%' (c "10");
    a "Tail" (v "U_2") '/' (c "10");
    a "Empty" (v "U_3") '=' (c "0");

    a "OUT" (v "Counter_1") '=' (c "0");
    a "Count_TMP" (v "Counter_2") '+' (c "3");

    (* T3 *)
    a "Top2" (v "Top") '*' (c "-2");

    (* T4 *)
    a "Counter" (v "Top2") '+' (vdelay "Count_TMP" 1);
    delay_widget "U" (v "Tail") 2;

    a "Counter_if_empty" (v "Empty") '-' (v "Counter_3");
    p "Counter_if_empty" "XX" "YY";
    a "OUT" (v "XX") '=' (c "1");
    a "OUT_1" (c "1") '#' (v "YY");
    a "OUT" (v "OUT_1") '-' (c "1");
    (*****)

    add_out "OUT";

    ()

  let prog = !program
  let () = make prog

  (* let () = Comp.Run_comp.run ~max:600 21122 42 prog *)

end

module R = M9()
