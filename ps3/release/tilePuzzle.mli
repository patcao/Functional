
type state
type move = N | S | E | W
type tile = int

include Solver.PUZZLE    with type state := state and type move := move
include Animation.PUZZLE with type state := state and type move := move

val of_list : int -> tile list -> state

