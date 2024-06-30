open Stdlib
open Compile

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
  let () = output (output_prog prog)
end

module M12 () = struct
  let () = reset ()

  let () =

    a "P3" (c "50") '*' (c "20");
    a "P3'" (c "50") '*' (c "20");
    a "P3''" (c "50") '*' (c "20");
    a "P6" (v "P3") '*' (v "P3'");
    a "P9" (v "P6") '*' (v "P3''");
    p3 "P9" "P9_1" "P9_2" "P9_3";
    a "P18" (v "P9_1") '*' (v "P9_2");

    a "P10" (v "P9_3") '*' (c "10");
    p "P10" "P10_1" "P10_2";

    a "2N" (v "N") '*' (c "2");
    p "2N" "2N_1" "2N_2";
    a "2NS1" (v "2N_1") '-' (c "1");
    a "2NS2" (v "2N_2") '-' (c "2");

    a "F" (v "2NS1") '*' (v "2NS2");

    a "A2" (c "A") '*' (c "A");

    a "A2S" (v "A2") '*' (v "S_1");

    a "P" (v "A2") '/' (c "F");

    a "S" (v "P10_1") '-' (v "P");

    p "S" "S_1" "S_2";

    

    add_out "Out"

  let prog = !program
  let () = output (output_prog prog)
end



module M_test () = struct

  let () = reset ()

  let () =
    equal_widet "S" (c "A") (c "B");
    add_out "S"
  ;;

  let prog = !program
  let () = output (output_prog prog)
end

module R = M12()
