
let w = 1000
let h = 1000

let norm (x, y, z) = sqrt (x*.x +. y*.y +. z*.z)
let (--) (x1, y1, z1) (x2, y2, z2) = x1 -. x2, y1 -. y2, z1 -. z2
let ( ** ) s (x, y, z) = s *. x, s *. y, s *. z
let (!+) = int_of_float

let eye ?(color=Graphics.black) mouse (px, py) radius =
  let open Graphics in
  let pos = (px, py, 0.) in
  let d = mouse -- pos in
  let x, y, _ = (radius *. 30. /. norm d) ** d in
  let x, y = px +. x, py +. y in
  Graphics.set_color black;
  draw_circle !+px !+py !+(radius *. 50.);
  Graphics.set_color color;
  fill_circle !+x !+y !+(radius *. 20.)

let init () =
  let open Graphics in
  open_graph (Printf.sprintf " %ix%i" w h);
  auto_synchronize false;

  let t0 = Unix.gettimeofday () in

  let blinky_eyes =
    List.init 20 (fun _ ->
        let x = Random.float (float w) in
        let y = Random.float (float h) in
        let s = 0.8 +. Random.float 0.4 in
        let f = 0.2 -. Random.float 0.5 in
        let v = 0.2 +. Random.float 0.5 in
        let dt = Random.float (Float.pi /. 2.) in
        let ft = 0.9 +. Random.float 0.2 in
        let r = max 0 (Random.int 250 - 100) in
        let g = max 0 (Random.int 200 - 100) in
        let b = max 0 (Random.int 150 - 100) in
        let color = Graphics.rgb r g b in
        fun mouse t ->
        eye ~color mouse (x +. 40. *. sin (ft *. 0.5 *. t), y +. 10. *. cos (ft *. 1.2 *. t)) (s *. (max 0. (f -. cos (dt +. v *. t))))
      )
  in

  while true do
    let x, y = mouse_pos () in
    let mouse = float x, float y, 100. in
    clear_graph ();
    let t = Unix.gettimeofday () -. t0 in
    List.iter (fun e -> e mouse t) blinky_eyes;
    synchronize ();
    Unix.sleepf (1./.60.);
  done

let () = init ()
