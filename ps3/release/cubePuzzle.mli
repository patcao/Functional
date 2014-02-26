
type state
type move = U | D | L | R | F | B

include Solver.PUZZLE    with type state := state and type move := move
include Animation.PUZZLE with type state := state and type move := move

val goal : state

