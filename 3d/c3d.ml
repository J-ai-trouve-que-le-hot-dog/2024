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

  let rounds = 20
  let srounds = string_of_int rounds
  let () =

    a "P3_1" (c "50") '*' (c "20");
    a "P3_2" (c "50") '*' (c "20");
    a "P3_3" (c "50") '*' (c "20");
    a "P6" (v "P3_1") '*' (v "P3_2");
    a "P9" (v "P3_3") '*' (v "P6");
    p3 "P9" "P9_1" "P9_2" "P9_3";
    a "P18" (v "P9_1") '*' (v "P9_2");
    a "P10" (v "P9_3") '*' (c "10");
    p "P10" "P10_1" "P10_2";

    p3 ~i:srounds "N" "N_1" "N_2" "N_3";

    a "S" (v "P10_1") '-' (v "P");
    p ~i:"0" "S" "S_1" "S_2";

    a "2N" (v "N_1") '*' (c "2");
    p "2N" "2N_1" "2N_2";
    a "2NS1" (v "2N_1") '-' (c "1");
    a "2NS2" (v "2N_2") '-' (c "2");

    a "F" (v "2NS1") '*' (v "2NS2");

    a "FSS" (v "F") '*' (v "P18");

    a "A2" (c "A") '*' (c "A");
    a "A2S" (v "A2") '*' (v "S_1");

    a "P" (v "A2S") '/' (v "FSS");

    a "SA" (v "S_2") '*' (c "A");
    a "SAR" (v "SA") '/' (v "P10_2");
    a "N1_t" (v "N_2") '=' (c "1");
    delay_widget "N1" (v "N1_t") 5;

    a "Out" (v "N1") '*' (v "SAR");

    a "N_t" (v "N_3") '-' (c "1");

    delay_widget "N" (v "N_t") 2;

    add_out "Out";

    ()

  let prog = !program
  let () = make prog

  (* let () = Comp.Run_comp.run ~max:200 1047197551 10 prog *)
end

(* module R = M12() *)

module M11 () = struct
  let () = reset ()

  let () =
    (* TODO init with loop?? *)
    a "P12_1" (c "64") '*' (c "64");
    a "P12_2" (c "64") '*' (c "64");
    a "P12_3" (c "64") '*' (c "64");
    a "P12_4" (c "64") '*' (c "64");
    a "P24_1" (v "P12_1") '*' (v "P12_2");
    a "P24_2" (v "P12_3") '*' (v "P12_4");
    a "P48" (v "P24_1") '*' (v "P24_2");
    p "P48" "P48_1" "P48_2";
    a "P96" (v "P48_1") '*' (v "P48_2");
    p "P96" "P96_1" "P96_2";
    a "P192" (v "P96_1") '*' (v "P96_2");
    p3 "P192" "P192_1" "P192_2" "P192_3";
    a "P384" (v "P192_1") '*' (v "P192_2");
    p "P384" "P384_1" "P384_2";
    a "P768" (v "P384_1") '*' (v "P384_2");
    p "P768" "P768_1" "P768_2";
    a "P1536" (v "P768_1") '*' (v "P768_2");
    p "P1536" "P1536_1" "P1536_2";
    a "P3072" (v "P1536_1") '*' (v "P1536_2");
    p "P3072" "P3072_1" "P3072_2";
    a "P6144" (v "P3072_1") '*' (v "P3072_2");
    p "P6144" "P6144_1" "P6144_2";
    a "P12288" (v "P6144_1") '*' (v "P6144_2");
    p "P12288" "P12288_1" "P12288_2";
    a "P24576" (v "P12288_1") '*' (v "P12288_2");
    p3 "P24576" "P24576_1" "P24576_2" "init_done";
    
    a "init_done0" (v "init_done") '*' (c "0");
    init_from ~init:"R" ~from:"P24576_1";
    init_from ~init:"U" ~from:"P24576_2";
    a "Y" (v "P192_3") '+' (c "-2");
    
    a "X" (v ~i:"A" "next_X") '+' (v "init_done0");   (*** 0 ***)
    p3 "X" "X_1" "X_2" "X_c";                         (*** 1 ***)
    p "X_c" "X_3" "X_4";                              (*** 2 ***)
    
    a "W2" (c "2") '+' (v "Y0");                      (*** 4 ***)
    p "W2" "W2_1" "W2_2";                             (*** 5 ***)
    a "Y0" (v "Y") '*' (v "Xmod2");                   (*** 3 ***)
    a "Xmod2" (v "X_1") '%' (c "2");                  (*** 2 ***)
 
    p3 "R" "R_1" "R_2" "R_c";                         (*** 0 ***)
    delay_widget "R_cd" (v "R_c") 4;                  (*** 4 ***)
    p "R_cd" "R_3" "R_4";                             (*** 5 ***)

    a "Rmul" (v "R_3") '*' (v "W2_1");                (*** 6 ***)
    a "Rdiv" (v "R_4") '/' (v "W2_2");                (*** 6 ***)
    p "Rdiv" "Rdiv_1" "Rdiv_2";                       (*** 7 ***)
    delay_widget "Rmuld" (v "Rmul") 1;                (*** 7 ***)
    a "Rdiff" (v "Rmuld") '-' (v "Rdiv_1");           (*** 8 ***)
    delay_widget "Rdiv_2d" (v "Rdiv_2") 2;            (*** 9 ***)
    a "Xmod10" (v "X_2") '%' (c "10");                (*** 2 ***)
    a "Xge3" (v "Xmod10") '/' (c "3");                (*** 3 ***)
    delay_widget "Xge3d" (v "Xge3") 5;                (*** 8 ***)
    a "Rcond" (v "Rdiff") '*' (v "Xge3d");            (*** 9 ***)
    a "R" (v "Rcond") '+' (v "Rdiv_2d");              (*** 10 ***)
    
    a "Xdiv" (v "X_3") '/' (c "10");                  (*** 3 ***)
    delay_widget "next_X" (v "Xdiv") 7;               (*** 10 ***)

    p "U" "U_1" "U_2";                                (*** 0 ***)
    a "Udiv" (v "U_1") '/' (v "R_1");                 (*** 1 ***)
    a "wasin" (v "Udiv") '%' (c "2");                 (*** 2 ***)
    a "wasnotin" (c "1") '-' (v "wasin");             (*** 3 ***)
    p "wasnotin" "wasnotin_1" "wasnotin_2";           (*** 4 ***)
    
    p ~i:"1" "C" "C_1" "C_2";                         (*** 0 ***)
    delay_widget "C_1d" (v "C_1") 4;                  (*** 4 ***)
    a "Cz" (v "C_1d") '+' (v "wasnotin_1");           (*** 5 ***)
    delay_widget "C" (v "Cz") 5;                      (*** 10 ***)
    delay_widget "R_2d" (v "R_2") 4;                  (*** 4 ***)
    a "Rm" (v "R_2d") '*' (v "wasnotin_2");           (*** 5 ***)
    delay_widget "U_2d" (v "U_2") 5;                  (*** 5 ***)
    a "Uz" (v "U_2d") '+' (v "Rm");                   (*** 6 ***)
    delay_widget "U" (v "Uz") 4;                      (*** 10 ***)

    delay_widget "X_4d" (v "X_4") 8;
    a "Xzero" (v "X_4d") '=' (c "0");
    a "S" (v "C_2") '+' (v "Xzero");

    add_out "S";
    ()


  let prog = !program
  let () = make prog

  (* let () = Comp.Run_comp.run ~max:300 33321411 10 prog *)
end

module R2 = M11 ()


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
