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

and binop = Add  (** [+] *)
