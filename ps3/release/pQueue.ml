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
  type 'a record = { tree : 'a tree; comparator : 'a comparator ; size : int}
  type 'a t = 'a record
  (*type 'a t = 'a tree * 'a comparator*)
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
    pq.size

  let is_empty pq   = pq.tree = Leaf

  let comparator pq = pq.comparator

  let empty    cmp  = {tree = Leaf ; comparator = cmp ; size = 0}

  let insert   x pq = 
  		let p = path_to_last ( pq.size + 1) in 
  		let rec helper x tree path = 
	  		match path with 
	  		| [] -> Node (Leaf,x,Leaf)
	  		| hd :: tl -> (
	  			match tree with 
	  			| Leaf -> Node (Leaf, x , Leaf)
	  			| Node (l,v,r) -> (
	  				match hd with
	  				| Left -> if (pq.comparator v x = Lt) then Node ( (helper v l tl), x , r)
	  											else Node ( (helper x l tl), v , r)

	  				| Right -> if (pq.comparator v x = Lt) then Node (l , x , (helper v r tl))
	  											else Node ( l, v , (helper x r tl))
	  				)
	  			)
  		in
  		{tree = (helper x (pq.tree) p ); comparator = pq.comparator ; size = pq.size + 1}

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
  			| [] -> Leaf
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
  								Node ( l, vr, repair (Node(l2,v,r2)) compar )
  							else tree
  							)
  				)
  			)
  		in 
  		let rec helper pq lastVal=   			
	  			let removedTree = 
	  				match (removeLastNode pq.tree (path_to_last pq.size)) with
	  				| Leaf -> Leaf
	  				| Node (l,v,r) -> Node(l,lastVal,r)
	  			in	  			
	  			repair removedTree pq.comparator
  		in
  		let getRootVal pq =   			
  				match pq.tree with
  				| Leaf -> failwith("Should not be seeing a Leaf here")
  				| Node (l,v,r) -> v
  		in
	if is_empty pq then None 
	  else 
		let lastVal = getLastNode pq.tree (path_to_last pq.size) in 
		 Some ( getRootVal pq ,  { tree = (helper pq lastVal) ; comparator =  pq.comparator ; size = pq.size - 1 }  ) 

  let max      pq   = 
                    match pq.tree with
                    | Leaf -> None
                    | Node (left,v,right) -> Some v                  
end

(******************************************************************************)
(** Heapsort ******************************************************************)
(******************************************************************************)
module Heapsort (PQ : PQ) = struct

  let rec to_list h = match PQ.remove h with
    | None        -> []
    | Some (x,h') -> x::to_list h'

  let rec of_list cmp l = List.fold_right (PQ.insert) l (PQ.empty cmp)

  let sort compare l =
    let cmp x y =
      let n = compare x y in
      if n < 0 then Lt
      else if n = 0 then Eq
      else Gt
    in

    to_list (of_list cmp l)

end

