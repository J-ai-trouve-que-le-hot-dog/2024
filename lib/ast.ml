open Common

module Encoded_string : sig
  type t

  val pp : Format.formatter -> t -> unit
  
  val from_raw_string : string -> t
  val to_raw_string : t -> string

  val from_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
    [@@deriving(show)]

  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n"

  let from_raw_string s = s
  let to_raw_string s = s

  let decode_char c =
    chars.[(Char.code c - 33)]

  let to_string s =
    String.map decode_char s

  let encode_char c =
    Char.chr (String.index_from chars 0 c + 33)

  let from_string s =
    String.map encode_char s
end

type expr =
  | Bool of bool  (** Booleans [T] *)
  | Int of Z.t  (** Integers [I] *)
  | String of Encoded_string.t  (** Strings [S] *)
  | Unop of unop * expr  (** Unary operators [U] *)
  | Binop of binop * expr * expr  (** Unary operators [B] *)
  | If of { cond : expr; tbranch : expr; fbranch : expr }  (** If [?] *)
  | Lambda of { var : int; body : expr }  (** Lambda abstractions [L] *)
  | Var of int

and unop =
  | Neg  (** [-] *)
  | Not  (** [!] *)
  | String_to_int  (** [#] *)
  | Int_to_string  (** [$] *)

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
  | Apply_strict (** [!] *)
  | Apply_mystery (** [~] *)

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
  | '!' -> Apply_strict
  | '~' -> Apply_mystery
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
  | Apply_strict -> '!'
  | Apply_mystery -> '~'

let rem s = String.sub s 1 (String.length s - 1)

let decode_int s =
  let r = ref Z.zero in
  for i = 0 to String.length s - 1 do
    let u = Char.code s.[i] - 33 in
    r := Z.(of_int 94 * !r + of_int u)
  done;
  !r

let rec parse get =
  let s = get () in
  match s.[0] with
  | 'T' -> Bool true
  | 'F' -> Bool false
  | 'I' -> Int (decode_int (rem s))
  | 'S' -> String (Encoded_string.from_raw_string (rem s))
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
    let v = Z.to_int (decode_int (rem s)) in
    Lambda { var = v; body = parse get }
  | 'v' ->
    let v = Z.to_int (decode_int (rem s)) in
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
  let parts =
    s
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "\n" && s <> " ")
  in
  parse_parts parts


let rec pp_expr ff e =
  match e with
  | Bool b -> Format.fprintf ff "%b" b
  | Int i -> Format.fprintf ff "%s" (Z.to_string i)
  | String s -> Format.fprintf ff "%S" (Encoded_string.to_string s)
  | Unop (u, e) -> Format.fprintf ff "U%c (%a)" (encode_unop u) pp_expr e
  | Binop (b, e1, e2) -> Format.fprintf ff "(%a) %c (%a)" pp_expr e1 (encode_binop b) pp_expr e2
  | If { cond; tbranch; fbranch } -> Format.fprintf ff "if %a then %a else %a" pp_expr cond pp_expr tbranch pp_expr fbranch
  | Lambda { var; body } -> Format.fprintf ff "(fun v%d -> %a)" var pp_expr body
  | Var v -> Format.fprintf ff "v%d" v

let int_to_encoded_string : Z.t -> string = fun n ->
  let r = ref n in
  let o = ref [] in
  while Z.(!r > zero) do
    o := Char.chr (33 + Z.(to_int (!r mod (of_int 94)))) :: !o;
    r := Z.(!r / of_int 94)
  done;
  if !o = [] then "!" else String.of_seq (List.to_seq !o)

let rec print_ast ff e =
  match e with
  | Bool b -> Format.fprintf ff (if b then "T" else "F")
  | Int i -> Format.fprintf ff "I%s" (int_to_encoded_string i)
  | String s -> Format.fprintf ff "S%s" (Encoded_string.to_raw_string s)
  | Unop (u, e) -> Format.fprintf ff "U%c %a" (encode_unop u) print_ast e
  | Binop (b, e1, e2) -> Format.fprintf ff "B%c %a %a" (encode_binop b) print_ast e1 print_ast e2
  | If { cond; tbranch; fbranch } -> Format.fprintf ff "? %a %a %a" print_ast cond print_ast tbranch print_ast fbranch
  | Lambda { var; body } -> Format.fprintf ff "L%s %a" (int_to_encoded_string (Z.of_int var)) print_ast body
  | Var v -> Format.fprintf ff "v%s" (int_to_encoded_string (Z.of_int v))

