open Format
    
let read_file file = In_channel.with_open_bin file In_channel.input_all

let base_url = "https://boundvariable.space/"
let cached_token = ref None

let get_token () : string =
  match !cached_token with
  | Some(t) -> t
  | None ->
    try
      let token = read_file ".token" in
      cached_token := Some(token);
      token
    with
    | Sys_error(_) as e ->
      eprintf "Please create a \".token\" file with the authentication token.\n";
      raise e

let make_headers () =
  [ ("Authorization", "Bearer " ^ get_token ()) ]

type scoreboard_t = (int * string) list
[@@deriving(show)]

let get_scoreboard () : scoreboard_t =
  let parse_scoreboard (j : Yojson.Basic.t) : scoreboard_t =
    let open Yojson.Basic.Util in
    let parse_row j
      =
      (j |> member "values" |> index 0 |> to_int,
       j |> member "values" |> index 1 |> to_string)
    in
    j
    |> member "rows"
    |> to_list
    |> List.map parse_row
  in
  let url = base_url ^ "scoreboard" in
  let r = Ezcurl.get ~url () in
  let r = Result.get_ok r in
  let json = Yojson.Basic.from_string r.body in
  parse_scoreboard json

let submit_todo (data_todo : string) : unit =
  let url = base_url ^ "submit" in
  let headers = make_headers () in
  let params = [Curl.CURLFORM_CONTENT ("data", data_todo, Curl.DEFAULT)] in
  let r = Ezcurl.post ~url ~headers ~params () in
  let _ = Result.get_ok r in
  ()
