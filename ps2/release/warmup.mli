(** This module contains a series of warmup exercises with the [List.fold_left]
    and [List.fold_right] functions. These exercises are intended to give
    familiarity with the basic principles of folding, a ubiquitous technique in
    functional programming. *)

(** The [sum] function returns the sum of the elements in a given list. More
    precisely, [sum [x0; ... ; xn] = x0 + ... + xn]. *)
val sum : int list -> int

(** The [rev] is an alias for the [List.rev] function which reverses a list.
    To expand on the specification in the
    {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html} official
    OCaml List module documentation}, we should have
    {[max2 [x0; ... ; xn] = [xn; ... ; x0]]} *)
val rev : 'a list -> 'a list

(** The [max2] function should return the second-greatest element in an
    ['a list] with respect to the canonical ordering given by
    [Pervasives.compare]. Your implementation should satisfy the following
    identity: {[List.length (List.filter ((>) (max2 xs))) = 1]} for all input
    lists [xs]. *)
val max2 : 'a list -> 'a

(** [all_pairs xs] should return the set-theoretic product of the list [xs] with
    itself. That is, for each [x,y] in [xs], the pair [(x,y)] should occur in
    [all_pairs xs]. *)
val all_pairs : 'a list -> ('a * 'a) list
