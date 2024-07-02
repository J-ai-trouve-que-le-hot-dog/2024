open Common

type term =
  | Bool of bool
  | Int of Z.t  
  | String of Rope.t
  | Unop of Ast.unop * term
  | Binop of Ast.binop * term * term
  | If of term * term * term
  | Lambda of term
  | Var of int

type value =
  | VBool of bool
  | VInt of Z.t
  | VString of Rope.t
  | VLambda of closure

and closure = term * env
              
and env = EnvEmpty | EnvSnoc of env * value Lazy.t

let pp_value ff = function
  | VBool b -> Format.fprintf ff "%b" b
  | VInt i -> Format.fprintf ff "%s" (Z.to_string i)
  | VString s -> Format.fprintf ff "%s" Ast.Encoded_string.(to_string (from_raw_string (Rope.to_string s)))
  | VLambda _ -> Format.fprintf ff "<closure>"

module IntMap = Map.Make(Int)

let term_from_expr =
  let rec go nvar (varmap : int IntMap.t) : Ast.expr -> term = function
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | String(s) -> String(Rope.of_string (Ast.Encoded_string.to_raw_string s))
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

let rec eval (env : env) (t : term) : value =
  eval_go env t
  
and eval_go (env : env) : term -> value = function
  | Bool(b) -> VBool(b)
  | Int(i) -> VInt(i)
  | String(s) -> VString(s)
  | Unop(o,x) -> eval_unop (o, eval env x)
  | Binop(Apply_strict,x,y)
  | Binop(Apply_mystery,x,y)
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
  | (Neg, VInt(x)) -> VInt Z.(-x)
  | (Not, VBool(b)) -> VBool(not b)
  | (Int_to_string, VInt(i)) -> VString(Rope.of_string (Ast.int_to_encoded_string i))
  | (String_to_int, VString(s)) -> VInt(Ast.decode_int (Rope.to_string s))
  | _ -> impossible __LOC__
           
and eval_binop : Ast.binop * value * value -> value = function
  | (Add, VInt(x), VInt(y)) -> VInt Z.(x + y)
  | (Sub, VInt(x), VInt(y)) -> VInt Z.(x - y)
  | (Mul, VInt(x), VInt(y)) -> VInt Z.(x * y)
  | (Div, VInt(x), VInt(y)) -> VInt Z.(x / y)
  | (Mod, VInt(x), VInt(y)) -> VInt Z.(x mod y)
  | (Lt, VInt(x), VInt(y)) -> VBool Z.(lt x y)
  | (Gt, VInt(x), VInt(y)) -> VBool Z.(gt x y)
  | (Eq, VInt(x), VInt(y)) -> VBool Z.(equal x y)
  | (Eq, VBool(x), VBool(y)) -> VBool (x = y)
  | (Eq, VString(x), VString(y)) -> VBool(Rope.equal x y)
  | (Or, VBool(x), VBool(y)) -> VBool(x || y)
  | (And, VBool(x), VBool(y)) -> VBool(x && y)
  | (Concat, VString(s), VString(t)) -> VString(Rope.concat2 s t)
  | (Take, VInt(i), VString(s)) -> 
    VString(Rope.sub s 0 (Z.to_int i))
  | (Drop, VInt(i), VString(s)) ->
    let i = Z.to_int i in
    VString(Rope.sub s i (Rope.length s - i))
  | (o,x,y) ->
    Format.printf "B%c %a %a@."
      Ast.(encode_binop o)
      pp_value x
      pp_value y
    ;
    impossible __LOC__

(* let t () = eval EnvEmpty (term_from_expr Decode.v) *)
