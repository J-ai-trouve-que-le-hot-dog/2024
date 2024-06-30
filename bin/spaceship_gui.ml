let read_file filename =
  let ic = open_in filename in
  let r = In_channel.input_all ic in
  close_in ic;
  r

let positions , start_moves =
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
  let start_moves = match _save_file with
    | Some(filename) -> begin
        try read_file filename
        with | Sys_error(_) -> ""
      end
    | None -> ""
  in
  positions , start_moves

let lmin l = List.fold_left min max_int l
let lmax l = List.fold_left max min_int l
let min_x = lmin (List.map fst positions)
let min_y = lmin (List.map snd positions)
let max_x = lmax (List.map fst positions)
let max_y = lmax (List.map snd positions)

let scale =
  let dx = max_x - min_x in
  let dy = max_y - min_y in
  let d = max dx dy in
  2000. /. float d

let shift = (float (-min_x + 1),float (-min_y + 1))

let scale = ref scale
let offset = ref shift

let ship_position = ref (0, 0)
let ship_speed = ref (0, 0)

let next_actions = ref []

type step = { position : int * int; speed : int * int; action : int * int }

let prev_path : step list ref = ref []

let () = Graphics.open_graph " 3840x2160"

let pos (x, y) =
  let (scale_x, scale_y) = !scale, !scale in
  int_of_float (scale_x *. (float x +. fst !offset)),
  int_of_float (scale_y *. (float y +. snd !offset))

let scale_factor = 1.5

let continue = ref true

let offset_and_last_button_press = ref None

let (--) (a,b) (c,d) = a-.c, b-.d
let (++) (a,b) (c,d) = a+.c, b+.d
let ( ** ) s (c,d) = s*.c, s*.d

module I = struct
  let (--) (a,b) (c,d) = a-c, b-d
  let (++) (a,b) (c,d) = a+c, b+d
  let ( ** ) s (c,d) = s*c, s*d
end

let set_action act =
  prev_path := { position = !ship_position; speed = !ship_speed; action = act } :: !prev_path;
  next_actions :=
    (match !next_actions with
     | [] -> []
     | _::t -> t);
  ship_speed := I.(act ++ !ship_speed);
  ship_position := I.(!ship_speed ++ !ship_position)


let potential_moves () =
  let acc = ref [] in
  for i = -1 to 1 do
    for j = -1 to 1 do
      let speed = I.(!ship_speed ++ (i, j)) in
      let pos = I.(!ship_position ++ speed) in
      acc:= ((i, j), pos) :: !acc
    done
  done;
  !acc

let auto () =

  let moves = potential_moves () in
  let moves =
    List.filter (fun (_, pos) ->
        if pos = !ship_position then false
        else
        List.mem pos positions)
      moves
  in
  match moves with
  | [act, _] ->
     set_action act
  | _ -> ()

let prev () =
  match !prev_path with
  | [] -> ()
  | { position; speed; action } :: prev ->
     prev_path := prev;
     ship_position := position;
     ship_speed := speed;
     next_actions := action :: !next_actions

let next () =
  match !next_actions with
  | [] -> ()
  | act :: _ ->
     set_action act

let save () =
  Printf.printf "\n\n\n%!";
  List.iter (fun { action = x, y; _ } ->
      let v = x + 3 * y + 5 in
      Printf.printf "%d" v)
    (List.rev !prev_path);
  Printf.printf "\n\n\n%!"

let rec ev_loop () =
  let st = Graphics.wait_next_event [Key_pressed; Mouse_motion; Button_down; Button_down] in
  if st.button then begin
      match !offset_and_last_button_press with
      | None ->
         offset_and_last_button_press := Some ((float st.mouse_x, float st.mouse_y), !offset);
      | Some (pos, prev_offset) ->
         let pos_now = (float st.mouse_x, float st.mouse_y) in
         offset := prev_offset ++ (1. /. !scale) ** (pos_now -- pos)
    end
  else offset_and_last_button_press := None;
  match st.key with
  | 'q' -> continue := false
  | '+' -> scale := !scale *. scale_factor
  | '-' -> scale := !scale /. scale_factor

  | 'r' -> set_action (-1, 1)
  | 't' -> set_action ( 0, 1)
  | 'y' -> set_action ( 1, 1)

  | 'f' -> set_action ( -1, 0)
  | 'g' -> set_action ( 0, 0)
  | 'h' -> set_action ( 1, 0)

  | 'v' -> set_action ( -1, -1)
  | 'b' -> set_action ( 0, -1)
  | 'n' -> set_action ( 1, -1)

  | 's' -> save ()

  | 'z' -> prev ()

  | 'e' -> next ()

  | ' ' -> auto ()

  | _ -> ()

let lineto p =
  let x, y = pos p in
  Graphics.lineto x y

let moveto p =
  let x, y = pos p in
  Graphics.moveto x y

let fill_circle p r =
  let x, y = pos p in
  Graphics.fill_circle x y r

let draw_prev_moves () =
  Graphics.set_color Graphics.magenta;
  moveto (!ship_position);
  List.iter (fun { position; _ } -> lineto position) !prev_path

let rec draw_ship_moves ~n ~pos ~speed ~acc =
  if n >= 0 then begin
      let next_action, acc =
        match acc with
        | [] -> (0,0), []
        | h :: t -> h, t
      in
      let speed = I.(speed ++ next_action) in
      let pos = I.(pos ++ speed) in
      Graphics.set_color Graphics.red;
      lineto pos;
      Graphics.set_color Graphics.green;
      fill_circle pos 2;
      draw_ship_moves ~n:(n-1) ~pos ~speed ~acc
    end

let draw_ship () =
  Graphics.set_color Graphics.red;

  (* let ship_x, ship_y = !ship_position in *)
  (* let vx, vy = !ship_speed in *)
  (* let dir = Float.atan2  *)

  let (cx, cy) = pos !ship_position in
  Graphics.fill_circle cx cy 4;

  draw_prev_moves ();

  moveto !ship_position;
  draw_ship_moves ~n:5 ~pos:!ship_position ~speed:!ship_speed ~acc:!next_actions;

  Graphics.set_color Graphics.blue;
  List.iter (fun (_, place) ->
      fill_circle place 3;
    ) (potential_moves ())

let draw () =
  Graphics.clear_graph ();

  let plot p =
    let x, y = pos p in
    Graphics.fill_rect (x-4) (y-4) 9 9
  in

  Graphics.set_color Graphics.black;
  List.iter plot positions;
  draw_ship ();
  ()

let decode_char = function
  | '5' -> (0,0)
  | '1' -> (-1,-1)
  | '2' -> (0,-1)
  | '3' -> (1,-1)
  | '6' -> (1,0)
  | '9' -> (1,1)
  | '8' -> (0,1)
  | '7' -> (-1,1)
  | '4' -> (-1,0)

let () =
  String.iter (fun c ->
    set_action (decode_char c)
  ) start_moves;
  
  Graphics.auto_synchronize false;
  let rec loop () =
    draw ();
    Graphics.synchronize ();
    ev_loop ();
    if !continue then loop ()
  in
  loop ()
