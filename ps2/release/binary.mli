(** The [Binary] module exports a function to add numbers in {{:
    http://en.wikipedia.org/wiki/Binary_number} binary}
    representations. When implementing this module you may find it
    useful to implement some helper functions. *)

(** The type [bit] represents a single binary digit *)
type bit = Zero | One

(** The type [bits] represents a sequence of binary digits. By
    convention, we will assume that the most significant bit at the
    head of the list *)
type bits = bit list

(** [bits_to_int lst] converts [bits] [lst] to an [int]. *)
val bits_to_int : bits -> int

(** [int_to_bits n] converts an [int] [n] to [bits]. *)
val int_to_bits : int -> bits

(** [normalize lst lst'] outputs [(lst'',lst''')] such that the
    [lst''] and [lst'''] have the same length. If the input lists are
    not the same length, the shorter is padded with [Zero]s. *)
val normalize : bits -> bits -> bits * bits

(** [drop_leading_zeros lst] returns [lst'] for 
    [lst = Zero::...::Zero::lst'], where [Zero::...::Zero] represents 
    all of the leading zeros in [lst]. *)
val drop_leading_zeros : bits -> bits

(** The [binary_addition m n] function is used to add two numbers [m]
    and [n] represented in binary using [bits].

    @return the binary representation of [m+n] in [bits], with all
    leading zeros removed. *)
val binary_addition : bits -> bits -> bits
