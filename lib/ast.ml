type expr =
  | Bool of bool  (** Booleans [T] *)
  | Int of int  (** Integers [I] *)
  | String of string  (** Strings [S] *)
  | Unop of unop * expr  (** Unary operators [U] *)
  | Binop of binop * expr  (** Unary operators [B] *)
  | If of { cond : expr; tbranch : expr; fbranch : expr }  (** If [?] *)
  | Lambda of { var : int; body : expr }  (** Lambda abstractions [L] *)

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

let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_char c =
  chars.[(Char.code c - 33)]

let decode_string s =
  String.map decode_char s

let decode s =
  if s.[0] = 'S' then
    Some (decode_string (String.sub s 1 (String.length s - 1)))
  else
    None

let encode_char c =
  Char.chr (String.index_from chars 0 c + 33)

let encode_string s =
  String.map encode_char s
