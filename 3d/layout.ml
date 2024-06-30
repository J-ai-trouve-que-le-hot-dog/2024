
let component_size component =
  Array.length component ,
  Array.fold_left max 0 (Array.map Array.length component)

let layout_with_sizes n m (components : string option array array list)
  : string option array array option =
  let components = Array.of_list components in
  let ncomponents = Array.length components in
  let nv = ref 0 in
  let new_var () = incr nv; !nv in
  let sizes = Array.map component_size components in
  let vars =
    Array.init ncomponents (fun i ->
        let (x,y) = sizes.(i) in
      Array.init (n+1 - x) (fun _ ->
          Array.init (m+1 - y) (fun _ ->
              new_var ()
            )
        )
    ) in
  let avars = Array.init ncomponents (fun _ ->
      CCVector.create ()
    ) in
  let at = Array.init n (fun _ ->
      Array.init m (fun _ ->
          CCVector.create ()
        )
    )
  in
  for c = 0 to ncomponents - 1 do
    let (x,y) = sizes.(c) in
    for i = 0 to n-x do
      for j = 0 to m-y do
        let v = vars.(c).(i).(j) in
        CCVector.push avars.(c) v;
        for di = 0 to x-1 do
          for dj = 0 to y-1 do
            if dj < Array.length (components.(c).(di)) 
            && Option.is_some (components.(c).(di).(dj))
            then
              CCVector.push at.(i+di).(j+dj) v
          done;
        done;
      done;
    done;
  done;
  try 
    let solver = Minisat.create () in
    let distinct l =
      for i = 0 to CCVector.length l - 1 do
        for j = 0 to i - 1 do
          let l1 = Minisat.Lit.(neg (make (CCVector.get l i))) in
          let l2 = Minisat.Lit.(neg (make (CCVector.get l j))) in
          Minisat.add_clause_l solver [ l1; l2 ]
        done;
      done
    in
    let unique l =
      distinct l;
      Minisat.add_clause_l solver
        (List.map (fun i -> Minisat.Lit.make i) (CCVector.to_list l));
      ()
    in
    for i = 0 to ncomponents-1 do
      unique avars.(i)
    done;
    for i = 0 to n-1 do
      for j = 0 to m-1 do
        distinct at.(i).(j)
      done;
    done;
    Minisat.solve solver;
    let out = Array.init n (fun _ -> Array.init m (fun _ -> None)) in
    for c = 0 to ncomponents - 1 do
      let (x,y) = sizes.(c) in
      for i = 0 to n-x do
        for j = 0 to m-y do
          let v = vars.(c).(i).(j) in
          if Minisat.value solver (Minisat.Lit.make v) = Minisat.V_true then
            begin
              for di = 0 to x-1 do
                for dj = 0 to y-1 do
                  if dj < Array.length (components.(c).(di)) 
                  && Option.is_some (components.(c).(di).(dj))
                  then begin
                    out.(i+di).(j+dj) <- components.(c).(di).(dj)
                  end
                done;
              done;
            end
        done;
      done;
    done;
    Some(out)
  with | Minisat.Unsat -> None

let layout (components : string option array array list) : string option array array =
  let exception Found of string option array array in
  let cx, cy =
    List.fold_left (fun (mx, my) c ->
        let mx',my' =  component_size c in
        max mx mx', max my my')
      (0, 0) components
  in
  try 
    for area = 1 to 1000000 do
      for n = cx to area do
        for m = cy to area do
          if area = n * m
          then 
            match layout_with_sizes n m components with
            | Some(x) -> raise (Found(x))
            | None -> ()
        done;
      done;
    done;
    failwith "not found"
  with
  | Found(x) -> x
  
