
let () =
  assert(Array.length Sys.argv = 2);
  let body = Sys.argv.(1) in
  let result = Api.communicate body in
  Format.printf "%s\n" result
