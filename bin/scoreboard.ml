
[@@@warning "-5"]
let _ = Spectrum.prepare_ppf Format.std_formatter

let () =
  let scoreboard = Api.get_scoreboard () in
  List.iter (fun row ->
      let open Api in
      if row.is_you
      then 
        Format.printf "@{<green>%3d %s@}\n"
          row.rank row.name
      else 
        Format.printf "%3d %s\n"
          row.rank row.name
    ) scoreboard;
  ()
