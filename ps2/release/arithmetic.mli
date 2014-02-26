(** This module defines an expression type for manipulating simple arithmetic
    expressions. The expressions are given by the type [exp] and the main
    operation defined is [fold], which generalizes [List.fold_right] to the
    [exp] type. *)

(** The [exp] type is the type abstraction for arithmetic expressions. *)
type exp =
| Val of int
(** The [Val] constructor represents primitive integers.*)
| Plus of exp * exp
(** The [Plus] constructor represents expressions of the form [x + y]. *)
| Times of exp * exp
(** The [Times] constructor represents expressions of the form [x * y]. *)

(** The expression [fold val_op plus_op times_op exp] recursively
    processes each sub-expression of [exp] via the following scheme:
    {ul {- It applies the operation [val_op] to each [Val] sub-expression.}
    {- It applies the operation [plus_op] to each [Plus] sub-expression.}
    {- It applies the operation [times_op] to each [Times] sub-expression}} *)
val exp_fold : (int -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> exp -> 'a

(** The [eval] function takes an input expression and evaluates it according
    the rules of ordinary integer arithmetic. *)
val eval : exp -> int

(** [to_string exp] return a fully parenthesized string representation of
    [exp] with no spaces between operators and their operands. *)
val to_string : exp -> string
