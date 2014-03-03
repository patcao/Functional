
open PQueue

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
    let findMove (src:state) (trgt:state) : move =                 
        let moves = (moves src) in
        List.fold_left (fun a x -> if (apply src x) = trgt then x else a) (List.hd moves) moves
    in        
    let rec helper path state seen = 
      if (is_goal state)                     then state::seen, Some [] 
      else if List.exists (equal state) seen then state::seen, None
      else(
        let rec nxtNode queue (seen : state list) =                        
                match (ListImpl.remove queue) with (*Most good state is in the first part of tuple*)
                | None -> seen, None
                | Some (s,q) -> 
                    match helper ((findMove state s) :: path) s seen with
                    | seen, Some path -> seen, Some ( (findMove state s)::path)
                    | seen, None -> nxtNode q seen
        in 
        let queue =  List.fold_left (fun a m -> ListImpl.insert (apply state m) a) (ListImpl.empty Puzzle.goodness) (moves state) 
        (*Add list of states to pQueue*)      
        in nxtNode queue (state::seen)
      )

    in snd (helper [] state [])
end
