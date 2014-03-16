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

let rec annotate (e : expr) (fvs : (id, typ) Hashtbl.t) : aexpr =
  failwith "I can't see anything. Fire anyway."

and annotate_op fvs bv = function
  | BinaryOp (op,l,r) -> begin
    match op with
    | Plus
    | Minus
    | Mult
    | Divide
    | Mod    -> failwith "Hold still and lemme shoot you."
    | Gt
    | Lt
    | Ltq
    | Gtq
    | Eq
    | Neq
    | And
    | Or     -> failwith "You guys don't give up!"
  end
  | UnaryOp (op,e)  -> failwith "There's one down!"
  | _ as e -> begin
    let msg = Printf.sprintf
      "The expression: %s\n is not an operator, but an operator was expected."
      (expr_to_string e) in
    runtime_error msg
  end

let add_constraint  t  t' u = (t,t')::u
let add_constraints cs u    = List.fold_left (fun a c -> c::a) u cs

let rec collect aexprs u = failwith "You're too slow! Time to end this!"

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
  | AUnaryOp (op,ae,t) -> failwith "TODO"
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
