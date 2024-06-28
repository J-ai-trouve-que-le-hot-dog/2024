
let () =
  assert(Array.length Sys.argv >= 2);
  let body = String.concat " " (List.tl (Array.to_list Sys.argv)) in
  let body = "get spaceship" ^ body in
  let body = "S" ^ Ast.Encoded_string.(to_raw_string (from_string body)) in
  let result = Api.communicate body in
  let msg = Ast.parse_input result in
  let positions = Spaceship.Parse.positions msg in
  Format.printf "positions@.@.%a@."
    Spaceship.Parse.pp positions
