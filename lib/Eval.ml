open Common

type term =
  | Bool of bool
  | Int of int  
  | String of string 
  | Unop of Ast.unop * term
  | Binop of Ast.binop * term * term
  | If of term * term * term
  | Lambda of term
  | Var of int
[@@deriving(show)]

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VLambda of closure

and closure = term * env
              
and env = EnvEmpty | EnvSnoc of env * value Lazy.t

let pp_value ff = function
  | VBool b -> Format.fprintf ff "%b" b
  | VInt i -> Format.fprintf ff "%d" i
  | VString s -> Format.fprintf ff "%s" Ast.Encoded_string.(to_string (from_raw_string s))
  | VLambda _ -> Format.fprintf ff "<closure>"

module IntMap = Map.Make(Int)

let term_from_expr =
  let rec go nvar (varmap : int IntMap.t) : Ast.expr -> term = function
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | String(s) -> String(Ast.Encoded_string.to_raw_string s)
    | Unop(o,t) -> Unop(o, go nvar varmap t)
    | Binop(b,x,y) ->
      Binop(b, go nvar varmap x, go nvar varmap y)
    | If(b) ->
      If(go nvar varmap b.cond, go nvar varmap b.tbranch, go nvar varmap b.fbranch)
    | Lambda(x) ->
      Lambda(go (nvar+1) (IntMap.add x.var nvar varmap) x.body)
    | Var(x) ->
      Var(nvar - 1 - IntMap.find x varmap)
  in go 0 IntMap.empty

let rec env_lookup : env * int -> value = function
  | EnvSnoc(_, v), 0 -> Lazy.force v
  | EnvSnoc(env, _), n -> env_lookup (env, n-1)
  | _ -> impossible __LOC__

let int_to_encoded_string : int -> string = fun n ->
  let r = ref n in
  let o = ref [] in
  while !r > 0 do
    o := Char.chr (33 + !r mod 94) :: !o;
    r := !r / 94
  done;
  if !o = [] then "!" else String.of_seq (List.to_seq !o)

let rec eval (env : env) (t : term) : value =
  eval_go env t
  
and eval_go (env : env) : term -> value = function
  | Bool(b) -> VBool(b)
  | Int(i) -> VInt(i)
  | String(s) -> VString(s)
  | Unop(o,x) -> eval_unop (o, eval env x)
  | Binop(Apply,x,y) -> apply (eval env x , lazy (eval env y))
  | Binop(o,x,y) -> eval_binop (o , eval env x , eval env y)
  | Lambda(t) -> VLambda(t, env)
  | If(b,x,y) ->
    begin match eval env b with
    | VBool(true)  -> eval env x
    | VBool(false) -> eval env y
    | _ -> impossible __LOC__
    end
  | Var(x) -> env_lookup (env, x)

and apply : value * value Lazy.t -> value = function
  | (VLambda(t, env), x) -> eval (EnvSnoc(env, x)) t
  | _ -> impossible __LOC__

and eval_unop : Ast.unop * value -> value = function
  | (Neg, VInt(x)) -> VInt(-x)
  | (Not, VBool(b)) -> VBool(not b)
  | (Int_to_string, VInt(i)) -> VString(int_to_encoded_string i)
  | (String_to_int, VString(s)) -> VInt(Ast.decode_int s)
  | _ -> impossible __LOC__
           
and eval_binop : Ast.binop * value * value -> value = function
  | (Add, VInt(x), VInt(y)) -> VInt(x + y)
  | (Sub, VInt(x), VInt(y)) -> VInt(x - y)
  | (Mul, VInt(x), VInt(y)) -> VInt(x * y)
  | (Div, VInt(x), VInt(y)) -> VInt(x / y)
  | (Mod, VInt(x), VInt(y)) -> VInt(x mod y)
  | (Lt, VInt(x), VInt(y)) -> VBool(x < y)
  | (Gt, VInt(x), VInt(y)) -> VBool(x > y)
  | (Eq, VInt(x), VInt(y)) -> VBool(x = y)
  | (Eq, VBool(x), VBool(y)) -> VBool(x = y)
  | (Eq, VString(x), VString(y)) -> VBool(x = y)
  | (Or, VBool(x), VBool(y)) -> VBool(x || y)
  | (And, VBool(x), VBool(y)) -> VBool(x && y)
  | (Concat, VString(s), VString(t)) -> VString(s ^ t)
  | (Take, VInt(i), VString(s)) -> 
    VString(String.sub s 0 i)
  | (Drop, VInt(i), VString(s)) ->
    VString(String.sub s i (String.length s - i))
  | (o,x,y) ->
    Format.printf "%a %a %a@."
      Ast.pp_binop o
      pp_value x
      pp_value y
    ;
    impossible __LOC__

(* let t () = eval EnvEmpty (term_from_expr Decode.v) *)
