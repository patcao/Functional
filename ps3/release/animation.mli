module type PUZZLE = sig
  include Solver.PUZZLE

  type animstate

  val init     : state     -> animstate
  val render   : animstate -> state -> (float*move) option -> unit
end

module Make (Puzzle : PUZZLE) : sig
  open Puzzle

  val run : state -> move list -> unit
end
 
