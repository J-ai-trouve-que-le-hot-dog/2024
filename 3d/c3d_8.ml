open Stdlib
open Comp.Compile

let init_program = { act = []; copies = []; outputs = []; to_init_from  = []; delay = [] }
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

let add_delay delay_to delay_from =
  program :=
    {
      !program with
      delay = { delay_from; delay_to = V delay_to } :: !program.delay;
    }

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
      add_delay out in_
    else
      let var = var () in
      add_delay var in_;
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

module M7 () = struct
  let () = reset ()

  let () =

    (* T1 *)
    p "V" ~i:"0" "V1" "V2";
    p3 "U" ~i:"A" "U1" "U3" "U4";
    p3 "Z" ~i:"2" "Z1" "Z2" "Z_c";


    (* T2 *)
    p3 "Z_c" "Z3" "Z4" "Z5";

    a "V10" (v "V1") '*' (v "Z1");
    a "Umod" (v "U1") '%' (v "Z2");
    p3 "U4" "U0_1" "U0_2" "U0_3";

    a "R" (c "A") '=' (v "V2");

    (* T3 *)
    a "S" (v "Z4") '#' (v "R");
    a "Ut" (vdelay "U3" 1) '/' (v "Z3");
    a "Vt" (v "V10") '+' (v "Umod");
    a "NZ" (v "Z5") '+' (c "1");
    a "U0a" (v "U0_1") '=' (c "0");
    a "U0a" (v "U0_2") '/' (v "U0_3");

    (* T4 *)
    p3 "U0a" "U0a_1" "U0a_2" "U0a_3";

    (* T5 *)
    a "Zx" (vdelay "NZ" 1) '-' (v "U0a_1");
    a "Vx" (vdelay "Vt" 1) '*' (v "U0a_2");
    a "U0a_A" (v "U0a_3") '*' (c "A");   
    a "Utx" (vdelay "Ut" 1) '+' (c "A");

    (* T6 *)
    (* If U = 0: Utx = A, U = A
       If U <> 0: Utx = U + A, U = U *)
    a "U" (v "Utx") '-' (v "U0a_A");
    delay_widget "V" (v "Vx") 1;
    delay_widget "Z" (v "Zx") 1;


    add_out "S"

  let prog = !program
  let () = make prog

  (* let () = Comp.Run_comp.run ~max:550 4 42 prog *)
end

module R = M7()
