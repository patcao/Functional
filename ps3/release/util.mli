(** Utility functions for problem set 3. *)

(** timing ********************************************************************)

(** [time f x] runs [f x] and returns the time it takes to run (in seconds). *)
val time : ('a -> 'b) -> 'a -> float

(** randomization *************************************************************)

(** [rand_list n l] returns a random list of [l] integers between [0] and [n] *)
val rand_list : int -> int -> int list

(** randomly shuffle a list. *)
val shuffle : 'a list -> 'a list

(** randomly select an element from a list *)
val choice : 'a list -> 'a

(** comparisons ***************************************************************)

(** Less than / Equal / Greater than *)
type comparison_result = Lt | Eq | Gt

(** An 'a comparator represents a total order on 'as. *)
type 'a comparator = 'a -> 'a -> comparison_result

(** printing ******************************************************************)

(** [string_of_list f l] returns a pretty joining of [f] invoked on every
 * element of [l]. *)
val string_of_list: ('a -> string) -> 'a list -> string
