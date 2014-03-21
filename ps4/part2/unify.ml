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

  let subOccurence (const : constr list) (x : id) (t : typ) accum : constr list= 
    
    let lst = List.fold_left (
                    fun a (t1,t2) -> 
                      if occurs x t1 then 
                        begin
                          if occurs x t2 then 
                            (subst t x t1, subst t x t2) :: a
                          else
                            (subst t x t1 , t2) :: a
                        end                      
                      else
                        begin
                          if occurs x t2 then 
                            (t1, subst t x t2) :: a
                          else
                            (t1 , t2) :: a
                        end
                      ) [] const in
    List.rev lst
  in


  let rec helper (const : constr list) accum = 
    match const with 
    | [] -> accum
    | (t1,t2) :: tl -> (        
          if t1 = t2 then helper tl accum else
            begin
              match (t1,t2) with
              | TVar(id),t2 -> (
                                if occurs id t2 then runtime_error "An error with TVar in t1"
                                  else helper (subOccurence tl id t2 []) ((id,t2) :: accum)
                                )
              | t1, TVar(id) -> (
                                if occurs id t1 then runtime_error "An error with TVar in t1"
                                  else helper (subOccurence tl id t1 []) ((id,t1) :: accum)
                                )
              | Arrow(t1,t2),Arrow(t3,t4) -> helper ((t1,t3) :: (t2,t4) :: tl) accum                                
              | TList(t1),TList(t2) -> helper ( (t1,t2) :: tl ) accum                                
              | _ -> runtime_error "none of the unification cases matched"
            end      
      )

  in 
  helper s []


