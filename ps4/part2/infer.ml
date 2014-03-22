open Ast
open Exceptions
open Printer

let char_count  = ref (Char.code 'a')
let alpha_count = ref 0

let reset_type_vars () =
  char_count  := Char.code 'a';
  alpha_count := 0

let rec next_type_var () : typ =
  let helper () =
    let result =
      ((String.make 1 (Char.chr (!char_count)))^
          (if !alpha_count = 0
           then ""
           else string_of_int (!alpha_count))) in
    if !char_count >= Char.code 'z' then begin
      char_count := Char.code 'a';
      incr alpha_count end
    else incr char_count;
    result in
  TVar (helper ())

let type_of = function
  | ABool       (_, a)
  | AInt        (_, a)
  | ANil        a
  | AUnit       a
  | ACons       (_, _, a)
  | AIfThenElse (_, _, _, a)
  | ALetRec     (_, _, _, _, a)
  | ALet        (_, _, _, _, a)
  | ABinaryOp   (_,_,_,a)
  | AUnaryOp    (_,_,a)
  | AVar        (_, a)
  | AFun        (_, _, a)
  | AApp        (_, _, a)
  | AMatch      (_, _, a) -> a

let type_of_pattern = function
  | APConstant (_, t)
  | APVar      (_,t)
  | APCons     (_, _, t) -> t

module VarSet = Set.Make (struct
  type t = id
  let compare = Pervasives.compare
end)

let alpha_vary (t : typ) : typ =
  let open VarSet in
  let rec collect_tvars tvars = function
    | TBool
    | TUnit
    | TInt          -> tvars
    | TList t'      -> collect_tvars tvars t'
    | TVar  id      -> add id tvars
    | Arrow (t1,t2) -> begin
      union (collect_tvars tvars t1) (collect_tvars tvars t2)
    end in
  let rec fresh_vars n acc =
    if n = 0 then List.rev acc else fresh_vars (n-1) (next_type_var()::acc) in
  let ids = collect_tvars VarSet.empty t in
  let s   =
    List.combine (VarSet.elements ids) (fresh_vars (VarSet.cardinal ids) []) in
  let rec replace s = function
    | TVar x      -> List.assoc x s
    | TBool       -> TBool
    | TInt        -> TInt
    | TUnit       -> TUnit
    | TList u     -> TList (replace s u)
    | Arrow (u,v) -> Arrow (replace s u, replace s v) in
  replace s t

let print_Annotate anExpr = 
begin 
  match anExpr with 
  | ABool(b,typ)       -> print_string "Bool";
  | AInt(num,type1) -> print_string "Int";
  | ANil(type1)     -> print_string "Nil";
  | AUnit(type1)    -> print_string "Unit";
  | ACons(e1,e2,type1) -> print_string "ACons";
  | AIfThenElse(e1,e2,e3,type1) -> print_string "IfElse";
  | ALetRec(id,type1,e1,e2,type2) -> print_string "LetRec";
  | ALet(id,typ1,e1,e2,typ2)      -> print_string "Let";
  | ABinaryOp(op,e1,e2,type1)      -> print_string "Binary";
  | AUnaryOp(op,e1,type1)          -> print_string "Unary";
  | AFun(id,e1,type1)              -> print_string "Fun";
  | AApp(e1,e2,type1)              -> print_string "App";
  | AVar (id,type1)                -> print_string "Var";
  | AMatch (e1,lst,typ)           -> print_string "Match";
end

let rec annotate (e : expr) (fvs : (id, typ) Hashtbl.t) : aexpr =
  match e with
  | Constant(const) -> 
              (match const with
                | Bool(b)  -> ABool(b,TBool)
                | Int(num) -> AInt(num,TInt)
                | Nil      -> ANil(TList(next_type_var ()))
                | Unit     -> AUnit(TUnit)
              )
  | BinaryOp(op,e1,e2) -> annotate_op fvs fvs e
  | UnaryOp(op,e1) -> annotate_op fvs fvs e
  | Var    (id) -> (try  AVar( id , (Hashtbl.find fvs id) ) with Not_found -> unbound_var id)                                        
  | Fun    (id,e1) ->
                      let nType = (next_type_var ()) in  
                      let hashCpy = Hashtbl.copy fvs in                     
                      Hashtbl.add hashCpy id nType; 
                      let aE1 = (annotate e1 hashCpy) in 
                      AFun(id, aE1, Arrow( (nType), (type_of aE1)) )
  | Cons   (e1,e2) -> let aE1 = annotate e1 fvs in 
                      let aE2 = annotate e2 fvs in 
                      (** type check list**)
                      ACons(aE1, aE2, TList(type_of aE1))            
  | IfThenElse (e1,e2,e3) -> AIfThenElse (annotate e1 fvs, 
                                            annotate e2 fvs, 
                                              annotate e3 fvs,
                                                 type_of (annotate e3 fvs))
                              (**Should make sure that e2 and e3 match in types **)  
  | Let      (id,e1,e2) -> 
                          let hshCpy = Hashtbl.copy fvs in                           
                           let aE1 = annotate e1 fvs in
                           Hashtbl.add hshCpy id (type_of aE1);
                           let aE2 = annotate e2 hshCpy in                           
                            ALet(id , (type_of aE1), aE1, aE2, (type_of aE2) )  
  | LetRec   (id,e1,e2) -> Hashtbl.add fvs id (next_type_var ());
                           Hashtbl.add fvs id (type_of (annotate e1 fvs));
                           ALetRec(id,Hashtbl.find fvs id, (annotate e1 fvs), 
                            (annotate e2 fvs), type_of(annotate e2 fvs))
  | App      (e1,e2) ->   (*begin                                               
                          match  (annotate e1 fvs) with
                          | AVar(id, Arrow(t1,t2)) ->                             
                            AApp  ( (annotate e1 fvs) ,(annotate e2 fvs) , t2)
                          | AFun(id, aE1, Arrow( appType , retType) ) ->                              
                                AApp (aE1, (annotate e2 fvs), retType)    
                          | AApp(e1,e2,t) -> AApp(e1,e2,t)
                          | _ -> runtime_error "This expression is not a function. Cannot be applied"
                        end*)
                        AApp(annotate e1 fvs,annotate e2 fvs, next_type_var())
  | Match     (e1,lst) ->   
           begin  
            let rec annotate_pattern pat bv = 
              match pat with
              | PConstant(const) -> (match const with 
                                      | Bool(b) -> APConstant(const, TBool)
                                      | Int (num) -> APConstant(const, TInt)
                                      | Nil -> APConstant(const, TUnit)
                                      | Unit -> APConstant(const, TUnit)
                                    )                                   
              | PVar(id) -> let nType = next_type_var () in  
                            Hashtbl.add bv id nType;
                            APVar(id , nType)
              | PCons (p1,p2) ->  let ap1 = annotate_pattern p1 bv in 
                                  APCons( ap1 , annotate_pattern p2 bv , type_of_pattern ap1)
            in   
            let rec helper lt acc = 
              match lt with
              | [] -> acc
              | (p1,exp1) :: tl -> 
                  let bv = Hashtbl.copy fvs in 
                  let ap = annotate_pattern p1 bv in
                  let ae = annotate exp1 bv in                  
                  helper tl ((ap,ae) :: acc)
            in
            let annotatedList = helper lst [] in 
            AMatch( (annotate e1 fvs) , annotatedList , type_of (snd (List.hd annotatedList)))
    end    
  (**of expr * (pattern * expr) list**)
and annotate_op fvs bv = function
  | BinaryOp (op,l,r) -> begin
    match op with
    | Plus                                   
    | Minus   
    | Mult    
    | Divide 
    | Mod    -> ABinaryOp (op, 
                            (annotate l fvs), 
                              (annotate r fvs), 
                                TInt )             
    | Gt
    | Lt
    | Ltq
    | Gtq
    | Eq
    | Neq
    | And
    | Or     -> ABinaryOp (op, 
                            (annotate l fvs), 
                              (annotate r fvs), 
                                TBool )   
  end
  | UnaryOp (op,e)  -> AUnaryOp(op, (annotate e fvs), TBool)   
  | _ as e -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (expr_to_string e) in
    runtime_error msg
  end

let add_constraint  t  t' u = (t,t')::u
let add_constraints cs u    = List.fold_left (fun a c -> c::a) u cs

let rec collect aexprs u = 
    match aexprs with
    | [] -> u
    | hd :: tl -> 
    begin
      match hd with
      | ABool(b , typ) -> collect tl u 
      | AInt (num , typ) -> collect tl u 
      | ANil (typ) -> collect tl u 
      | AUnit(typ) -> collect tl u 
      | ACons(e1,e2,typ) -> collect (e1 :: tl)
                              ( 
                              ( type_of e2 , (TList(type_of e1)) ) ::
                               u)
                              (*(  ((type_of e1), TList (type_of e1)) :: u  )*)
      | AIfThenElse (e1,e2,e3,typ) -> let constrList = 
                                          (( type_of e1, TBool ) ::
                                            ( type_of e2, type_of e3 ) ::
                                              u) in
                                      collect (e1 :: e2 :: e3 :: tl) constrList
      | ABinaryOp (op,e1,e2,typ) -> collect_operator_constraints tl u hd
      | AUnaryOp   (op,e1,typ) -> collect_operator_constraints tl u hd
      | ALetRec (id,typ1,e1,e2,typ2) -> collect tl ((typ1 , type_of e1) :: u)
      | ALet    (id,typ1,e1,e2,typ2) -> collect tl ((typ1 , type_of e1) :: u)
      | AFun    (id,e1,typ) ->          collect (e1 :: tl) u
      | AApp    (e1,e2,typ) ->        
                                      print_string (aexpr_to_string e1);
                                      ( match (type_of e1) with
                                        | Arrow (argType,retType) -> 
                                          collect (e1 :: e2 :: tl) 
                                            ( (argType ,type_of e2) :: u)                                       
                                        | x -> print_string ("\n type is " ^ (type_to_string x) ^ "\n");[]
                                          (*failwith ("Something went wrong in collect")*)
                                      )
      | AVar   (id, typ) -> collect tl u 
      | AMatch  (e1,lst, typ) ->      
          let rec helper  lt u = 
              match lt with 
              | [] -> u
              | (pat,exp) :: tl ->  
                    ( match pat with
                      | APCons (p1,p2,typ1) -> helper tl ( 
                                     (type_of_pattern p2, (TList(type_of_pattern p1)) ) ::
                                     (type_of e1 , type_of_pattern p2) :: u)
                      | APConstant (c,t) -> helper tl ( (type_of e1 , type_of_pattern pat) :: u)
                      | APVar  (id,t) -> helper tl ( (type_of e1 , type_of_pattern pat) :: u)
                    )
          in
          collect tl (helper lst u)
          (*of aexpr * ((apattern * aexpr) list) * typ*)      
    end

and collect_operator_constraints aexprs u = function
  | ABinaryOp (op,l,r,t) -> begin
    match op with
    | Plus
    | Minus
    | Mult
    | Divide
    | Mod    -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1, TInt)::(t2, TInt)::u)
    end
    | And
    | Or     -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1,TBool) :: (t2,TBool) :: u)
    end
    | Gt
    | Lt
    | Ltq
    | Gtq
    | Eq
    | Neq    -> begin
      let (t1,t2) = (type_of l, type_of r) in
      collect (l::r::aexprs) ((t1,t2) :: u)
    end
  end
  | AUnaryOp (op,ae,t) -> collect (ae :: aexprs) ((type_of ae,TBool) :: u)
  | _ as ae -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (aexpr_to_string ae) in
    runtime_error msg
  end

let infer (e : expr) (fvs : (id, typ) Hashtbl.t) : typ =
  let ae = annotate e fvs in
  debug_print "annotated expression:";
  debug_print (aexpr_to_string ae);
  let cl = collect [ae] [] in
  debug_print "constraints:";
  debug_print (constraints_to_string cl);
  let s = Unify.unify cl in
  debug_print "substitution:";
  debug_print (subst_to_string s);
  let t = (Unify.apply s (type_of ae)) in
  debug_print "type before alpha varying:";
  debug_print (type_to_string t);
  reset_type_vars ();
  alpha_vary t
