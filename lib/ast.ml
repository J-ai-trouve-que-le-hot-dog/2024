open Common

type expr =
  | Bool of bool  (** Booleans [T] *)
  | Int of int  (** Integers [I] *)
  | String of string  (** Strings [S] *)
  | Unop of unop * expr  (** Unary operators [U] *)
  | Binop of binop * expr * expr  (** Unary operators [B] *)
  | If of { cond : expr; tbranch : expr; fbranch : expr }  (** If [?] *)
  | Lambda of { var : int; body : expr }  (** Lambda abstractions [L] *)
  | Var of int
[@@deriving(show)]

and unop =
  | Neg  (** [-] *)
  | Not  (** [!] *)
  | String_to_int  (** [#] *)
  | Int_to_string  (** [$] *)
[@@deriving(show)]

and binop =
  | Add  (** [+] *)
  | Sub  (** [-] *)
  | Mul  (** [*] *)
  | Div  (** [/] *)
  | Mod  (** [%] *)
  | Lt  (** [<] *)
  | Gt  (** [>] *)
  | Eq  (** [=] *)
  | Or  (** [|] *)
  | And  (** [&] *)
  | Concat  (** [.] *)
  | Take  (** [T] *)
  | Drop  (** [D] *)
  | Apply  (** [$] *)
[@@deriving(show)]

let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_char c =
  chars.[(Char.code c - 33)]

let decode_string s =
  String.map decode_char s

let encode_char c =
  Char.chr (String.index_from chars 0 c + 33)

let encode_string s =
  String.map encode_char s

let decode_unop : char -> unop = function
  | '-' -> Neg
  | '!' -> Not
  | '#' -> String_to_int
  | '$' -> Int_to_string
  | _ -> impossible __LOC__

let encode_unop : unop -> char = function
  | Neg -> '-'
  | Not -> '!'
  | String_to_int -> '#'
  | Int_to_string -> '$'

let decode_binop : char -> binop = function
  | '+' -> Add
  | '-' -> Sub
  | '*' -> Mul
  | '/' -> Div
  | '%' -> Mod
  | '<' -> Lt
  | '>' -> Gt
  | '=' -> Eq
  | '|' -> Or
  | '&' -> And
  | '.' -> Concat
  | 'T' -> Take
  | 'D' -> Drop
  | '$' -> Apply
  | _ -> impossible __LOC__

let encode_binop : binop -> char = function
  | Add -> '+'
  | Sub -> '-'
  | Mul -> '*'
  | Div -> '/'
  | Mod -> '%'
  | Lt -> '<'
  | Gt -> '>'
  | Eq -> '='
  | Or -> '|'
  | And -> '&'
  | Concat -> '.'
  | Take -> 'T'
  | Drop -> 'D'
  | Apply -> '$'

let rem s = String.sub s 1 (String.length s - 1)

let decode_int s =
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    r := 94 * !r + Char.code s.[i] - 33
  done;
  !r

let rec parse get =
  let s = get () in
  match s.[0] with
  | 'T' -> Bool true
  | 'F' -> Bool false
  | 'I' -> Int (decode_int (rem s))
  | 'S' -> String (decode_string s)
  | 'U' -> Unop (decode_unop s.[1], parse get)
  | 'B' ->
    let e1 = parse get in
    let e2 = parse get in
    Binop (decode_binop s.[1], e1, e2)
  | '?' ->
    let c = parse get in
    let e1 = parse get in
    let e2 = parse get in
    If { cond = c; tbranch = e1; fbranch = e2 }
  | 'L' ->
    let v = decode_int (rem s) in
    Lambda { var = v; body = parse get }
  | 'v' ->
    let v = decode_int (rem s) in
    Var v
  | _ -> Bool false

let parse_parts parts =
  let l = ref parts in
  let get () =
    match !l with
    | [] -> assert false
    | h :: r -> l := r; h
  in
  let r = parse get in
  if !l <> [] then
    Format.eprintf "Parse problem: remaining parts@.";
  r

let parse_input s =
  let parts = String.split_on_char ' ' s in
  parse_parts parts
