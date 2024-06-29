open Graphics


let lmin l =
  List.fold_left min max_int l
let lmax l =
  List.fold_left max min_int l

let rec wait_q () =
  let st = wait_next_event [Key_pressed] in
  match st.key with
  | 'q' -> ()
  | _ -> wait_q ()

let pixels = true

let show positions =
  let min_x = lmin (List.map fst positions) in
  let min_y = lmin (List.map snd positions) in
  let max_x = lmax (List.map fst positions) in
  let max_y = lmax (List.map snd positions) in

  let scale =
    let dx = max_x - min_x in
    let dy = max_y - min_y in
    let d = max dx dy in
    2000. /. float d
  in
  let scale_x = scale in
  let scale_y = scale in
  let shift = (-min_x + 1, -min_y + 1) in

  let pos (x, y) =
  int_of_float (scale_x *. float (x + fst shift)),
  int_of_float (scale_y *. float (y + snd shift))
  in

  let moveto p =
    let x, y = pos p in
    moveto x y
  in

  let lineto p =
    let x, y = pos p in
    lineto x y
  in

  (* let plot p = *)
  (*   let x, y = pos p in *)
  (*   plot x y *)
  (* in *)

  let plot p =
    let x, y = pos p in
    fill_rect (x-1) (y-1) 3 3
  in

  open_graph " 2000x2000";
  Graphics.clear_graph ();

  if not pixels then begin
      moveto (List.hd positions);
      List.iter lineto (List.tl positions)
    end
  else begin
      List.iter plot positions
    end;
  wait_q ()


let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "get spaceship" ^ body in
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string body)) in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  let positions = Spaceship.Parse.positions msg in
  Format.printf "positions@.@.%a@."
    Spaceship.Parse.pp positions;
  show positions
