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

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUnop of Ast.unop
  | VLambda of closure

and closure = term * env
              
and env = EnvEmpty | EnvSnoc of env * value Lazy.t

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
  | _ -> todo __LOC__
