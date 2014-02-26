(** This module contains utility functions for the [list] data
    type. In these exercises you will be implementing functions that
    manipulate [list]s in a variety of ways. You may find the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html} OCaml
    List module documentation} useful while you devise your
    solutions. *)

(** [lengths_rec l] uses recursion to output a list [l'] such
    that the [n]{^ th} element of [l'] is the length of the [n]{^ th}
    element of [l]. *)
val lengths_rec : 'a list list -> int list

(** [lengths_fold] is identical to [lengths_rec], but uses a folding
    function from the [List] module instead of recursion. *)
val lengths_fold : 'a list list -> int list

(** [lengths_lib] is identical to [lengths_fold], but uses functions other
    than those in the folding section of the [List] module. *)
val lengths_lib : 'a list list -> int list

(** [find_first_value_rec lst x] uses recursion to produce [Some z] 
    for the first pair [(y,z] in the list such that [x] equals [y]. 
    Return [None] if no such pair exists.*)
val find_first_value_rec : ('a * 'b) list -> 'a -> 'b option

(** [find_first_value_fold] is identical to [find_first_value_rec], but
    uses a folding function from the [List] module instead of recursion. *)
val find_first_value_fold : ('a * 'b) list -> 'a -> 'b option

(** [find_first_value_lib] is identical to [find_first_value_fold], but 
    uses functions other than those in the folding section of the [List] 
    module. *)
val find_first_value_lib : ('a * 'b) list -> 'a -> 'b option

(** [confirm_outputs i o fs] outputs [true] if and only if each
    function [f] in [fs] returns output [o] when applies to 
    input [i]. *)
val confirm_outputs : ('a -> 'b) list -> 'a -> 'b -> bool

(** [total_length lst] outputs the total length of all lists 
    elements of [lst]. *)
val total_length : 'a list list -> int

(** [find_last_value_rec lst x] evaluates to [Some z] for the last pair 
    [(y,z] in the list such that [x] equals [y]. Return [None] if no such 
    pair exists.*)
val find_last_value : ('a * 'b) list -> 'a -> 'b option

(** [median lst] returns [None] if [lst] is the empty list,
    and otherwise returns [Some x], where [x] is the median
    of an odd-length list or the lesser of the two middle
    elements of an even-length list. The ordering is determined
    by the pervasive polymorphic comparison operator, [(<)]. *)
val median : 'a list -> 'a option
