
open Tsdl

let read_file filename =
  let ic = open_in filename in
  let r = In_channel.input_all ic in
  close_in ic;
  r

let positions =
  assert(Array.length Sys.argv >= 2);
  let filename = Sys.argv.(1) in
  let _save_file =
    if Array.length Sys.argv > 2 then
      Some Sys.argv.(2)
    else
      None
  in
  let stage = read_file filename in
  let positions = Spaceship.Parse.positions_from_string stage in
  positions

let scale = ref 1.
let offset = ref (0., 0.)

let () = Graphics.open_graph " 3840x2160"

let lmin l = List.fold_left min max_int l
let lmax l = List.fold_left max min_int l
let min_x = lmin (List.map fst positions)
let min_y = lmin (List.map snd positions)
let max_x = lmax (List.map fst positions)
let max_y = lmax (List.map snd positions)

let pos (x, y) =
  let (scale_x, scale_y) = !scale, !scale in
  int_of_float (scale_x *. (float x +. fst !offset)),
  int_of_float (scale_y *. (float y +. snd !offset))

let scale_factor = 1.1

let continue = ref true

let offset_and_last_button_press = ref None

let (--) (a,b) (c,d) = a-.c, b-.d
let (++) (a,b) (c,d) = a+.c, b+.d
let ( ** ) s (c,d) = s*.c, s*.d

let rec ev_loop () =
  let st = Graphics.wait_next_event [Key_pressed; Mouse_motion; Button_down; Button_down] in
  if st.button then begin
      match !offset_and_last_button_press with
      | None ->
         offset_and_last_button_press := Some ((float st.mouse_x, float st.mouse_y), !offset);
      | Some (pos, prev_offset) ->
         Format.printf "ICI@.";
         let pos_now = (float st.mouse_x, float st.mouse_y) in
         offset := prev_offset ++ (1. /. !scale) ** (pos_now -- pos)
    end
  else offset_and_last_button_press := None;
  match st.key with
  | 'q' -> continue := false
  | '+' -> scale := !scale *. scale_factor
  | '-' -> scale := !scale /. scale_factor
  | _ -> ()


let draw () =
  Graphics.clear_graph ();

  let plot p =
    let x, y = pos p in
    Graphics.fill_rect (x-1) (y-1) 3 3
  in

  List.iter plot positions


let () =
  let rec loop () =
    draw ();
    ev_loop ();
    if !continue then loop ()
  in
  loop ()
