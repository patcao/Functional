
open PQueue
open Util

(** See solver.mli *)
module type PUZZLE = sig
  type state
  type move

  val apply: state -> move -> state
  val moves: state -> move list
  val is_goal: state -> bool
  val equal: state -> state -> bool
  val goodness: state Util.comparator
end

(** See solver.mli *)
module type S = functor (Puzzle:PUZZLE) -> sig
  val solve : Puzzle.state -> Puzzle.move list option
end

(** A depth-first implementation of the Solver interface *)
module MakeUnguided (Puzzle:PUZZLE) = struct
  open Puzzle

  let solve state =
    (* returns a list of states traversed and optionally a solution *)
    let rec helper path state seen =
      if      is_goal state                  then state::seen, Some []
      else if List.exists (equal state) seen then state::seen, None
      else let rec loop moves seen = match moves with
        | []          -> seen, None
        | move::moves -> 
            match helper (move::path) (apply state move) seen with
           | seen, Some path -> seen, Some (move::path)
           | seen, None      -> loop moves seen
      in loop (moves state) (state::seen)
    in snd (helper [] state [])
end

module Make (Puzzle:PUZZLE) = struct
  open Puzzle
  
  let solve state : Puzzle.move list option =    
      (*Performs some initial tasks like creating the pQueue*)    
      let initial state : move list option =

        let rec helper state path queue seen pathLength: move list option=
        if is_goal state then Some path
      else(
          (*Add all the adjacent states that haven't been seen yet*)
          (*Fold across a list of moves. Want to put in <state>,<path>,<path length>*)
          let queue = List.fold_left 
          (fun a e ->  if List.exists (equal (apply state e)) seen then a
                        else HeapImpl.insert ( (apply state e),e :: path, pathLength + 1) a
                        ) 
          queue (moves state) in 
          (*Pop the next best node off the stack*)
          match (HeapImpl.remove queue) with
          | None -> None
          | Some ( (s,p, pLength) , q) -> 
                  helper s p q (state :: seen) pLength                  
          )
        in
          (*Comparator for the priority queue*)
          let compar (s1,p1,pL1) (s2,p2,pL2) = match goodness s1 s2 with
                                                    | Gt -> Gt
                                                    | Lt -> Lt 
                                                    | Eq -> 
                                                      if pL1 < pL2
                                                      then Gt else Lt
          in
          (*Queue stores a tuple of <state>,<path to initial>m <path length>*)   
          let queue = HeapImpl.empty (compar) in        
          match (helper state [] queue [] 0) with
          | None -> None
          | Some p -> Some (List.rev p)
      in
    initial state
end

   



