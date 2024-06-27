
let () =
  let scoreboard = Api.get_scoreboard () in
  List.iter (fun (rank, name) ->
      Format.printf "%3d %s\n" rank name
    ) scoreboard;
  ()
