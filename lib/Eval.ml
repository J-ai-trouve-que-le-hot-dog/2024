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
  | VUnop of Ast.unop
  | VLambda of closure
[@@deriving(show)]

and closure = term * env
[@@deriving(show)]
              
and env = EnvEmpty | EnvSnoc of env * value Lazy.t
[@@deriving(show)]

module IntMap = Map.Make(Int)

let term_from_expr =
  let rec go nvar (varmap : int IntMap.t) : Ast.expr -> term = function
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | String(s) -> String(s)
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

let rec eval (env : env) : term -> value = function
  | Bool(b) -> VBool(b)
  | Int(i) -> VInt(i)
  | String(s) -> VString(s)
  | Unop(o,x) -> eval_unop (o, eval env x)
  | Binop(o,x,y) -> eval_binop (o , eval env x , eval env y)
  | Lambda(t) -> VLambda(t, env)
  | If(b,x,y) ->
    begin match eval env b with
    | VBool(true)  -> eval env x
    | VBool(false) -> eval env y
    | _ -> impossible __LOC__
    end
  | Var(x) -> env_lookup (env, x)

and eval_unop : Ast.unop * value -> value = function
  | _ -> todo __LOC__
           
and eval_binop : Ast.binop * value * value -> value = function
  | (Add, VInt(x), VInt(y)) -> VInt(x + y)
  | (Sub, VInt(x), VInt(y)) -> VInt(x - y)
  | (Mul, VInt(x), VInt(y)) -> VInt(x * y)
  | (Div, VInt(x), VInt(y)) -> VInt(x / y)
  | (Mod, VInt(x), VInt(y)) -> VInt(x mod y)
  | (Lt, VInt(x), VInt(y)) -> VBool(x < y)
  | (Gt, VInt(x), VInt(y)) -> VBool(x > y)
  | (Eq, VInt(x), VInt(y)) -> VBool(x = y)
  | (Or, VBool(x), VBool(y)) -> VBool(x || y)
  | (And, VBool(x), VBool(y)) -> VBool(x && y)
  | (Concat, VString(s), VString(t)) -> VString(s ^ t)
  | (Take, VInt(i), VString(s)) -> 
    VString(String.sub s 0 i)
  | (Drop, VInt(i), VString(s)) ->
    VString(String.sub s i (String.length s - i))
  | (Apply, VLambda(t, env), v) -> eval (EnvSnoc(env, lazy v)) t
  | (o,x,y) ->
    Format.printf "%a@."
      Ast.pp_binop o;
    impossible __LOC__