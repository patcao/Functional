open Ast
open Exceptions
open Printer

let rec occurs (x : id) (t : typ) : bool =
  match t with
    | TBool
    | TUnit
    | TInt         -> false
    | TList t      -> occurs x t
    | TVar y       -> x = y
    | Arrow (u, v) -> occurs x u || occurs x v

let rec subst (s : typ) (x : id) = function
  | TBool        -> TBool
  | TInt         -> TInt
  | TUnit        -> TUnit
  | TList t'     -> TList (subst s x t')
  | TVar y as t  -> if x = y then s else t
  | Arrow (u, v) -> Arrow (subst s x u, subst s x v)

let apply (s : substitution) (t : typ) : typ =
  List.fold_right (fun (x, e) -> subst e x) s t

let unify (s : constr list) : substitution =
  failwith "Never give up. Trust your instincts."
