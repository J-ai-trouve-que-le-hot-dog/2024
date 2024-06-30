open Stdlib
open Comp.Compile

let init_program = { act = []; copies = []; outputs = [] }
let program = ref init_program
let count = ref 0
let reset () =
  program := init_program;
  count := 0
let v ?(i = "") s = Var (V s, i)
let c s = Const s

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


let a = add_act
let p = add_copy
let p3 = add_copy_3

module M7 () = struct
  let () = reset ()

  let () =
    p "V" ~i:"0" "V1" "V2";
    p "U" ~i:"A" "U1" "U2";

    p "U2" "U3" "U4";

    a "U" (v "U3") '/' (c "10");
    a "V10" (v "V1") '*' (c "10");

    a "Umod" (v "U1") '%' (c "10");

    a "V" (v "V10") '+' (v "Umod");

    a "U0" (v "U4") '=' (c "0");

    a "R" (c "A") '-' (v "V2");

    p3 "R" "R1" "R2" "R3";

    a "Z" (v "R1") '=' (c "0");

    a "Z" (v "R2") '/' (v "R3");

    a "U0'" (c "1") '-' (v "U0");

    a "S" (v "U0'") '-' (v "Z");

    add_out "S"

  let prog = !program
  let () = make prog
end

(* module R = M7() *)

(* module M10 () = struct *)

(*   let () = *)

    

(*     () *)

(*   let prog = !program *)
(*   (\* let () = make prog *\) *)

(*   let () = Comp.Run_comp.run ~max:200 50000000 20 prog *)

(* end *)

(* module R = M10() *)

module M12 () = struct
  let () = reset ()

  let rounds = 10
  let srounds = string_of_int rounds
  let () =

    a "P3" (c "50") '*' (c "20");
    a "P3'" (c "50") '*' (c "20");
    a "P3''" (c "50") '*' (c "20");
    a "P6" (v "P3") '*' (v "P3'");
    a "P9" (v "P6") '*' (v "P3''");
    a "P18" (v "P9_1") '*' (v "P9_2");

    p3 ~i:srounds "N" "N_1" "N_2" "N'";
    p "N'" "N_3" "N_4";

    equal_widet "N10" (v "N_2") (c srounds);
    p "N10" "N_not_Init" "N_10'";
    a "N_is_Init" (c "1") '-' (v "N_10'");
    a "S_Init" (v "P9") '*' (v "N_is_Init");

    a "S_d" (v "S_Init") '+' (v "S_loop" ~i:"0");

    a "S" (v "S_d") '#' (c "0");

    a "P" (v "A2S") '/' (v "FSS");

    a "S_loop" (v "P9") '-' (v "P");

    a "2N" (v "N_1") '*' (c "2");
    p "2N" "2N_1" "2N_2";
    a "2NS1" (v "2N_1") '-' (c "1");
    a "2NS2" (v "2N_2") '-' (c "2");

    a "F" (v "2NS1") '*' (v "2NS2");

    a "FSS'" (v "F") '*' (v "P18");

    a "FSS''" (v "FSS'") '*' (c "10");
    a "FSS" (v "FSS''") '*' (c "10");

    a "A2" (c "A") '*' (c "A");
    a "A2S'" (v "A2") '*' (v "S_1");
    a "A2S" (v "A2S'") '*' (c "10");

    p "S" "S_1" "S_2";

    a "SA" (v "S_2") '*' (c "A");

    a "N1_t" (v "N_3") '=' (c "1");

    delay_widget "N1" (v "N1_t") 3;

    a "Out'" (v "N1") '*' (v "SA");

    a "Out" (v "Out'") '/' (v "P9");

    a "N_t" (v "N_4") '-' (c "1");

    delay_widget "N" (v "N_t") 5;

    add_out "Out";

    ()

  let prog = !program
  (* let () = make prog *)

  let () = Comp.Run_comp.run ~max:200 50000000 20 prog
end

module R = M12()


module M_test () = struct

  let () = reset ()

  let () =
    equal_widet "U" (c "A") (c "B");
    a "S" (v "U") '-' (c "12");
    add_out "S"
  ;;

  let prog = !program
  let () = make prog

  let () = Comp.Run_comp.run 10 10 prog
end

(* module R = M_test() *)
