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

type scoreboard_row =
  { is_you : bool;
    rank : int;
    name : string;
  }
[@@deriving(show)]

type scoreboard = scoreboard_row list
[@@deriving(show)]

let get_scoreboard () : scoreboard =
  let parse_scoreboard (j : Yojson.Basic.t) : scoreboard =
    let open Yojson.Basic.Util in
    let parse_row j : scoreboard_row
      =
      { is_you = j |> member "isYou" |> to_bool;
        rank = j |> member "values" |> index 0 |> to_int;
        name = j |> member "values" |> index 1 |> to_string
      }
    in
    j
    |> member "rows"
    |> to_list
    |> List.map parse_row
  in
  let url = base_url ^ "scoreboard" in
  let headers = make_headers () in
  let r = Ezcurl.get ~url ~headers () in
  let r = Result.get_ok r in
  let json = Yojson.Basic.from_string r.body in
  parse_scoreboard json

let communicate (body : string) : string =
  let url = base_url ^ "communicate" in
  let headers = make_headers () in
  let params = [] in
  let content = `String(body) in
  let r = Ezcurl.post ~url ~headers ~params ~content () in
  let r = Result.get_ok r in
  r.body

let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_char c =
  chars.[(Char.code c - 33)]

let decode_string s =
  String.map decode_char s

let decode s =
  if s.[0] = 'S' then
    Some (decode_string s)
  else
    None

let encode_char c =
  Char.chr (String.index_from chars 0 c + 33)

let encode_string s =
  String.map encode_char s
