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

let decode s =
  if s.[0] = 'S' then
    Some (decode_string (String.sub s 1 (String.length s - 1)))
  else
    None

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


