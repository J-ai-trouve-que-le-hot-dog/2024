
let component_size component =
  Array.length component ,
  Array.fold_left max 0 (Array.map Array.length component)

let shape : string option array array -> _ = Array.map (Array.map Option.is_some)

let layout_with_sizes n m (components : string option array array list)
  : string option array array option =

  let shapes = CCVector.create () in
  let shape_count = CCVector.create () in
  let shape_components = CCVector.create () in
  let components = Array.of_list components in
  
  Array.iter (fun c ->
      let i = ref 0 in
      while !i < CCVector.length shapes && CCVector.get shapes !i <> shape c do
        incr i
      done;
      if !i = CCVector.length shapes then
        begin
          CCVector.push shapes (shape c);
          CCVector.push shape_count 0;
          CCVector.push shape_components (CCVector.create ())
        end;
      CCVector.set shape_count !i (CCVector.get shape_count !i + 1);
      CCVector.push (CCVector.get shape_components !i) c
    )
    components;
  
  let nshapes = CCVector.length shapes in
  let nv = ref 0 in
  let new_var () = incr nv; !nv in
  let sizes = CCVector.map component_size shapes in
  let vars =
    Array.init nshapes (fun i ->
        let (x,y) = CCVector.get sizes i in
      Array.init (n+1 - x) (fun _ ->
          Array.init (m+1 - y) (fun _ ->
              new_var ()
            )
        )
    ) in
  let avars = Array.init nshapes (fun _ ->
      CCVector.create ()
    ) in
  let at = Array.init n (fun _ ->
      Array.init m (fun _ ->
          CCVector.create ()
        )
    )
  in
  for c = 0 to nshapes - 1 do
    let (x,y) = CCVector.get sizes c in
    for i = 0 to n-x do
      for j = 0 to m-y do
        let v = vars.(c).(i).(j) in
        CCVector.push avars.(c) v;
        for di = 0 to x-1 do
          for dj = 0 to y-1 do
            if dj < Array.length ((CCVector.get shapes c).(di)) 
            && ((CCVector.get shapes c).(di).(dj))
            then
              CCVector.push at.(i+di).(j+dj) v
          done;
        done;
      done;
    done;
  done;
  Format.eprintf "Variables: %d@." !nv;
  try
    let nclauses = ref 0 in
    let solver = Minisat.create () in
    (* Minisat.set_verbose solver 2; *)
    let distinct_n2 l =
      for i = 0 to CCVector.length l - 1 do
        for j = 0 to i - 1 do
          incr nclauses;
          let l1 = Minisat.Lit.(neg (make (CCVector.get l i))) in
          let l2 = Minisat.Lit.(neg (make (CCVector.get l j))) in
          Minisat.add_clause_l solver [ l1; l2 ]
        done;
      done
    in
    let rec distinct vs =
      if CCVector.length vs > 1 then begin
        let ws = CCVector.create () in
        CCVector.iter (fun i ->
            if CCVector.is_empty ws
            || CCVector.length (CCVector.get ws (CCVector.length ws - 1)) = 3
            then CCVector.push ws (CCVector.create ());
            CCVector.push (CCVector.get ws (CCVector.length ws - 1)) i
          ) vs;
        let nvs = CCVector.create () in
        CCVector.iter (fun w ->
            let vw = new_var () in
            distinct_n2 w;
            CCVector.iter (fun x ->
                let l1 = Minisat.Lit.(neg (make x)) in
                let l2 = Minisat.Lit.(make vw) in
                Minisat.add_clause_l solver [ l1; l2 ]
              ) w;
            CCVector.push nvs vw
          ) ws;
        distinct nvs
      end
    in
    let add_clause l =
      let go l =
        if l > 0
        then Minisat.Lit.make l
        else Minisat.Lit.(neg (make (-l)))
      in
      incr nclauses;
      Minisat.add_clause_l solver (List.map go l)
    in
    let at_least count l =
      let l = CCVector.to_array l in
      if count > Array.length l then raise(Minisat.Unsat);
      let sz = Array.length l in
      let vars = Array.init (sz+1) (fun _ -> Array.init (count+1) (fun _ -> new_var ())) in
      (* vars.(i).(j)   iff  the sum of l(1)..l(i-1) is j *)
      for i = 0 to sz-1 do
        for j = 0 to count - 1 do
          add_clause [ - vars.(i).(j) ; - l.(i) ;  + vars.(i+1).(j+1) ];
          add_clause [ - vars.(i).(j) ; - l.(i) ;  - vars.(i+1).(j) ];
          add_clause [ - vars.(i).(j) ; + l.(i) ;  + vars.(i+1).(j) ];
          add_clause [ - vars.(i).(j) ; + l.(i) ;  - vars.(i+1).(j+1) ];
          add_clause [ - vars.(i+1).(j+1) ; vars.(i).(j) ; vars.(i).(j+1) ];
          ()
        done;
      done;
      add_clause [ vars.(0).(0) ];
      for j = 1 to count do
        add_clause [ - vars.(0).(j) ];
      done;
      add_clause [ vars.(sz).(count) ];
      ()
    in
    for i = 0 to nshapes-1 do
      at_least (CCVector.get shape_count i) avars.(i)
    done;
    for i = 0 to n-1 do
      for j = 0 to m-1 do
        distinct at.(i).(j)
      done;
    done;
    Format.eprintf "Clauses: %d@." !nclauses;
    Minisat.simplify solver;
    Minisat.solve solver;
    let out = Array.init n (fun _ -> Array.init m (fun _ -> None)) in
    for c = 0 to nshapes - 1 do
      let (x,y) = CCVector.get sizes c in
      for i = 0 to n-x do
        for j = 0 to m-y do
          let v = vars.(c).(i).(j) in
          if Minisat.value solver (Minisat.Lit.make v) = Minisat.V_true then
            begin
              let comp = CCVector.pop_exn (CCVector.get shape_components c) in
              for di = 0 to x-1 do
                for dj = 0 to y-1 do
                  if dj < Array.length ((CCVector.get shapes c).(di)) 
                  && ((CCVector.get shapes c).(di).(dj))
                  then begin
                    out.(i+di).(j+dj) <- comp.(di).(dj)
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
  Format.eprintf "Layout@.";
  Format.eprintf "Num components: %d@." (List.length components);
  Format.eprintf "Total component area: %d@."
    (List.fold_left (fun acc comp ->
      let cs = ref 0 in Array.iter (Array.iter (fun cc -> if cc <> None then cs := !cs + 1)) comp;
      acc + !cs) 0 components);
  let exception Found of string option array array in
  let cx, cy =
    List.fold_left (fun (mx, my) c ->
        let mx',my' =  component_size c in
        max mx mx', max my my')
      (20, 20) components
  in
  try 
    for area = 1500 to 10000 do
      Format.eprintf "Trying area %d@." area;
      for n = cx to area do
        for m = cy to area do
          if area = n * m
          then begin
            Format.eprintf "Trying %d = %d * %d@." area n m;
            match layout_with_sizes n m components with
            | Some(x) -> raise (Found(x)) (*Format.eprintf "FOUND@."*)
            | None -> ()
          end
        done;
      done;
    done;
    failwith "not found"
  with
  | Found(x) -> x
  
