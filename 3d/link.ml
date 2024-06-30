let find_labels t =
  let acc = ref [] in
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t.(0) - 1 do
      if String.contains t.(i).(j) ':' then begin
        let [label; data] = String.split_on_char ':' t.(i).(j) in
        acc := (label, (i, j)) :: !acc;
        t.(i).(j) <- data
      end
    done
  done;
  !acc

let resolve_labels t labels =
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t.(0) - 1 do
      let s = t.(i).(j) in
      if s <> "@" && String.contains s '@' then begin
        match String.split_on_char '@' s with
        | [label; "x"] ->
          let (li, lj) =
            try List.assoc label labels
            with Not_found ->
              failwith (Printf.sprintf "Missing %s" label)
          in
          t.(i).(j) <- string_of_int (j - lj + 1)
        | [label; "y"] ->
          let (li, lj) = try List.assoc label labels
            with Not_found ->
              failwith (Printf.sprintf "Missing %s" label)
          in
          t.(i).(j) <- string_of_int (i - li)
        | _ -> failwith ("bad label: " ^ s)
      end
    done
  done


let pad t =
  let maxw =
    Array.fold_right (fun l -> max (Array.length l))
      t 0
  in
  Array.map (fun l ->
      let w = Array.length l in
      if w = maxw then l
      else
        Array.init maxw (fun i ->
            if i < Array.length l then l.(i) else "."))
    t

let parse s =
  let l = String.split_on_char '\n' s in
  List.filter_map
    (function
      | "" -> None
      | s ->
        Option.some
        @@ (String.trim s
           |> String.split_on_char ' '
           |> List.map String.trim
           |> List.filter (function "" -> false | _ -> true)
           |> Array.of_list))
    l
  |> Array.of_list
  |> pad


let read_file ic =
  let rec loop acc =
    try
      let s = input_line ic in
      loop (s :: acc)
    with _ -> String.concat "\n" (List.rev acc)
  in
  loop []


let pad n s =
  let n = n - String.length s in
  s ^ String.init n (fun _ -> ' ')

let pad_left n s =
  let n = n - String.length s in
  String.init n (fun _ -> ' ') ^ s

let map f m = Array.map (Array.map f) m

let c_width = 3

let main () =
  let ic = open_in Sys.argv.(1) in
  let firstline = input_line ic in
  let a = read_file ic |> parse in
  let labels = find_labels a in
  resolve_labels a labels;
  print_endline firstline;
  for j = 0 to Array.length a - 1 do
    for i = 0 to Array.length a.(0) - 1 do
      print_string (pad_left (1+c_width) (if a.(j).(i) = "" then "." else a.(j).(i)))
    done;
    print_string "\n";
  done

let () = main ()
  
