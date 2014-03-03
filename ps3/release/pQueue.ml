open Util

(** See pQueue.mli *)
module type PQ = sig

  type 'a t
  val empty    : 'a comparator -> 'a t
  val insert   : 'a -> 'a t -> 'a t
  val remove   : 'a t -> ('a * 'a t) option
  val max      : 'a t -> 'a option
  val size     : 'a t -> int
  val is_empty : 'a t -> bool

  val comparator : 'a t -> 'a comparator

end

(******************************************************************************)
(** List-backed priority queue implementation *********************************)
(******************************************************************************)
module ListImpl = struct
  (* invariant: the list is sorted by comparator (max is at head *)
  type 'a t = 'a list * 'a comparator

  let empty cmp = [], cmp

  let insert x (l, cmp) =
    let rec insert l = match l with
      | [] -> [x]
      | y::xs when cmp x y = Lt -> y::(insert xs)
      | _ -> x::l
    in (insert l, cmp)

  let remove (l, cmp) = match l with
    | []    -> None
    | x::xs -> Some (x, (xs,cmp))

  let max (l, _) = match l with
    | []    -> None
    | x::xs -> Some x

  let size     (l, _) = List.length l
  let is_empty (l, _) = l = []

  let comparator (_,cmp) = cmp
end

(******************************************************************************)
(** Heap-backed priority queue implementation *********************************)
(******************************************************************************)
module HeapImpl = struct
	
  type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
  type 'a t = 'a tree * 'a comparator
  type dir = Left | Right

  (** returns the path from the root to the last node in a complete binary tree
      with size s *)
  let rec path_to_last s =
    let rec helper a s =
      if s < 1 then failwith "tree is empty"
      else if s = 1 then a
      else let dir = if s land 1 = 0 then Left else Right in
           helper (dir::a) (s lsr 1)
    in helper [] s

  let size     pq   = 
    let rec helper tree = 
      match tree with
      | Leaf -> 0
      | Node (left,v,right) -> 1 + helper left + helper right
    in
    	helper (fst pq)

  let is_empty pq   = (fst pq) = Leaf

  let comparator pq = snd pq

  let empty    cmp  = (Leaf,cmp)

  let insert   x pq = 
  		let p = path_to_last ((size pq) + 1) in 
  		let rec helper x tree path = 
	  		match path with 
	  		| [] -> Node (Leaf,x,Leaf)
	  		| hd :: tl -> (
	  			match tree with 
	  			| Leaf -> Node (Leaf, x , Leaf)
	  			| Node (l,v,r) -> (
	  				match hd with
	  				| Left -> if ((snd pq) v x = Lt) then Node ( (helper v l tl), x , r)
	  											else Node ( (helper x l tl), v , r)

	  				| Right -> if ((snd pq) v x = Lt) then Node (l , x , (helper v r tl))
	  											else Node ( l, v , (helper x l tl))
	  				)
	  			)
  		in
  		((helper x (fst pq) p ), snd pq)

  let remove   pq   =   	
  		(*Gets the value in the last node of the tree*)	
  		let rec getLastNode (tree : 'a tree) (path : dir list) : 'a = 
  			match path with 
  			| [] -> (
  				match tree with 
  				| Leaf -> failwith "Should not have reached a leaf"
  				| Node (l,v,r) -> v
  				)
  			| hd :: tl ->(
  				match tree with 
  				| Leaf -> failwith "Should not have reached a leaf"
  				| Node (l,v,r) -> 
	  				if hd = Left then getLastNode l tl
	  				else getLastNode r tl
  			)
  		in
  		(*Returns tree with the last rightmost node removed*)
  		let rec removeLastNode (tree : 'a tree) (path : dir list) : 'a tree =   			
  			match path with 
  			| [] -> failwith "Should not have given an empty path"
  			| hd :: hd2 :: tl ->(
  				match tree with 
  				| Leaf -> failwith "Should not have reached a leaf"
  				| Node (l,v,r) -> 
	  				if hd = Left then  Node (removeLastNode l (hd2 :: tl) , v , r)
	  				else Node ( l , v , removeLastNode r (hd2 :: tl) )
	  			)
  			| hd :: tl -> (
  				match tree with 
  				| Leaf -> failwith "Should not have reached a leaf"
  				| Node (l,v,r) -> 
  					if hd = Left then Node ( Leaf, v , r)
  					else Node ( l , v , Leaf)
  				)  			
  		in
  		(*Repairs the heap property of the tree*)
  		let rec repair tree compar = 
  			match tree with 
  			| Leaf -> Leaf
  			| Node(l,v,r) -> (
  				match r with
  				| Leaf -> ( (*Right node is empty*)
  					match l with
  					| Leaf -> tree
  					| Node( l1 ,vl,r1 ) -> if(compar vl v = Gt) then (*Only Left tree has a node*)
  												Node ( repair (Node(l1,v,r1)) compar , vl, r)
  											else
  												tree
  				)

  				| Node ( l2,vr,r2) -> ( (*Right node has a value*)
  					match l with
  					| Leaf -> failwith("Should not have a right branch without a left")
  					| Node (l3,vl,r3) -> 
  						if (compar vl vr = Gt) then (*Left value greater than right*)
  							(if(compar vl v = Gt) then (*left value greater than root*)
  								Node ( repair (Node(l3,v,r3)) compar, vl, r)
  							else
  								tree (*Current Node is in correct place*)
  							)
  						else (*Right node greater than left*)
  							(	
  							if(compar vr v = Gt) then (*Right is larger than root*)
  								Node ( l, vr, (Node(l2,v,r2)))
  							else tree
  							)
  				)
  			)
  		in 
  		let rec helper pq lastVal=   			
	  			let removedTree = 
	  				match removeLastNode (fst pq) (path_to_last (size pq)) with
	  				| Leaf -> Leaf
	  				| Node (l,v,r) -> Node(l,lastVal,r)
	  			in
	  			repair removedTree (snd pq)
  		in
	  		if is_empty pq then None 
	  		else 
		  		let lastVal = getLastNode (fst pq) (path_to_last (size pq)) in 
		  		Some ( lastVal,  ((helper pq lastVal) , (snd pq)) ) 

  let max      pq   = 
                    match (fst pq) with
                    | Leaf -> None
                    | Node (left,v,right) -> Some v
   let traverse pq =    
	  let rec preOrder tree = 
	    match tree with
	    | Leaf -> []
	    | Node (left,v,right) -> v :: (preOrder left ) @ (preOrder right)
	  in
	    preOrder (fst pq)

end

(******************************************************************************)
(** Heapsort ******************************************************************)
(******************************************************************************)
module Heapsort (PQ : PQ) = struct

  let rec to_list h = match PQ.remove h with
    | None        -> []
    | Some (x,h') -> x::to_list h'

  let rec of_list cmp l = List.fold_right PQ.insert l (PQ.empty cmp)

  let sort compare l =
    let cmp x y =
      let n = compare x y in
      if n < 0 then Lt
      else if n = 0 then Eq
      else Gt
    in

    to_list (of_list cmp l)

end

