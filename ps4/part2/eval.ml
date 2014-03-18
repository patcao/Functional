open Ast
open Exceptions
open Printer

let rec vcompare v1 v2 = match v1,v2 with
  | VBool b1       , VBool b2        -> compare b1 b2
  | VInt  n1       , VInt n2         -> compare n1 n2
  | VNil           , VNil            -> 0
  | VCons _        , VNil            -> 1
  | VNil           , VCons _         -> -1
  | VCons (x1, xs1), VCons (x2, xs2) -> begin
    let c = compare x1 x2 in
    if c = 0 then vcompare xs1 xs2 else c
  end
  | _ -> begin
    let msg = Printf.sprintf
      "The comparison of the values %s and %s resulted in failure."
      (val_to_string v1)
      (val_to_string v2) in
    runtime_error msg
  end

let concat (env1 : environment) (env2 : environment) : environment =
  IdMap.fold (fun x v e -> IdMap.add x v e) env1 env2

let rec pattern_match (v : value) (p : pattern) : bool * environment =
    let emptyMap = IdMap.empty in 
    match p with 
    | PConstant(const)-> (
                              match (const,v) with 
                              | Bool(b1),VBool(b2) -> b1 = b2, emptyMap
                              | Int(num1),VInt(num2) -> num1 = num2, emptyMap
                              | Nil, VNil -> (true, emptyMap)
                              | Unit,VUnit -> (true, emptyMap)
                              | _ -> (false,emptyMap)
                          )    
    | PVar(id) ->         (true , IdMap.add id (ref v) emptyMap)

    | PCons(p1,p2) -> match v with
                      VCons ( v1,v2 )-> (
                                          match (pattern_match v1 p1),(pattern_match v2 p2)  with
                                          | (b1,en1),(b2,en2) -> if b1 && b2 then (true , concat en1 en2)
                                                                  else (false,emptyMap)
                                        )                 

                      | _ -> (false,emptyMap)

let update (x : id) (v : value) (env : environment) : unit =
  try (IdMap.find x env) := v with Not_found -> ()

let rec eval (e : expr) (env : environment) : value =
  match e with
  | Constant(c)->( match c with
                   | Bool(b) -> VBool(b)
                   | Int(n) -> VInt(n)
                   | Nil -> VNil
                   | Unit -> VUnit
                )
  | Var(vID) -> (try !(IdMap.find vID env) with Not_found -> failwith("Variable was not found in the environment"^vID))
  | Fun (id,e1) -> VClosure (env,id,e1)
  | BinaryOp (op,e1,e2) -> eval_operator env (BinaryOp(op,e1,e2))
  | UnaryOp (op,e1) ->  eval_operator env (UnaryOp(op,e1))
  | Cons (e1,e2) -> VCons(eval e1 env, eval e2 env)
  | IfThenElse (e1,e2,e3) ->(
                            match (eval e1 env) with
                            | VBool(true) -> (eval e2 env)
                            | VBool(false) -> (eval e3 env)
                            | _ -> failwith("type error in if then else") 
                          )
  | Let(id,e1,e2) -> (*update id (eval e1 env) env;*) eval e2 (IdMap.add id (ref (eval e1 env)) env)
  | LetRec (id,e1,e2) -> (let env' = (IdMap.add id (ref VUndef) env) in update id (eval e1 env') env'; eval e2 env')
  | App (e1,e2) ->(
                    match (eval e1 env) with
                    | VClosure (envi,id,exp) -> eval exp (IdMap.add id (ref(eval e2 env)) envi)
                    | _ -> failwith ("Evaluation of function did not give back a VClosure")
                  )
  | Match (e2,lst) ->        
            let v2 = eval e2 env in 
            let rec helper l = 
              match l with
              [] -> failwith ("Pattern matching was not exhaustive")
              | (pat,exp) :: tl -> (
                      match (pattern_match v2 pat) with
                      | (b , en) -> if b then eval exp (concat en env) else helper tl
                    )
            in
            helper lst
            (** NOTE: WORK OUT SCOPING OF VARIABLES **)
 (**let rec fact x = match x with 1 -> 1 | x -> x * (fact (x-1))**)

and eval_operator env = function
  | BinaryOp (op,e1,e2) -> begin
    match op with
    | Plus   -> eval_arith e1 e2 env (+)
    | Minus  -> eval_arith e1 e2 env (-)
    | Mult   -> eval_arith e1 e2 env ( * )
    | Divide -> eval_arith e1 e2 env (/)
    | Mod    -> eval_arith e1 e2 env (mod)
    | And    -> eval_bool  e1 e2 env (&&)
    | Or     -> eval_bool  e1 e2 env (||)
    | Eq     -> eval_comp e1 e2 env (=)
    | Neq    -> eval_comp e1 e2 env (<>)
    | Gt     -> eval_comp e1 e2 env (>)
    | Lt     -> eval_comp e1 e2 env (<)
    | Gtq    -> eval_comp e1 e2 env (>=)
    | Ltq    -> eval_comp e1 e2 env (<=)
  end
  | UnaryOp (op,e) -> (
                    match (eval e env) with
                    | VBool(true) -> VBool(false)
                    | VBool(false) -> VBool(true)
                    | _ -> failwith ("Unary operator can only be applied to boolean")
                  )
  | _ as e -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (expr_to_string e) in
    runtime_error msg
  end

and eval_arith e e' env op = match eval e env, eval e' env with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | VInt _, _ -> type_error e' TInt
  | _ -> type_error e TInt

and eval_bool e e' env op = match eval e env, eval e' env with
  | (VBool b1, VBool b2) -> VBool (op b1 b2)
  | VBool _, _ -> type_error e' TBool
  | _ -> type_error e TBool

and eval_comp e e' env op = VBool (op (vcompare (eval e env) (eval e' env)) 0)
