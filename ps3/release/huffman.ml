
open Util
open PQueue

type 'a hufftree = Leaf of 'a | Node of 'a hufftree * 'a hufftree
type 'a encoding = Empty | Tree of 'a hufftree
type bit = Zero | One


let build_tree chars = 
	let deoption a = 
		match a with
		| None -> failwith "Cannot deOption"
		| Some x -> x 
	in
	(*Creates a list containing a tuple that holds (<element> , <frequency>) *)
	let countedList = 
	List.fold_left 
		(fun a e -> if( List.exists (fun x -> fst x = e) a) then 
						(e , (List.assoc e a) + 1) :: List.remove_assoc e a
					else
						(e,1) :: a
		) [] chars
	in 
	(*If frequency of 1 node is less than that of the other it is considerd Gt because it is better*)
	let queue =  (* (<tree> , <frequency>) PQueue.ListImpl *)
		List.fold_left 
		(fun a e -> ListImpl.insert (Leaf(fst e),snd e) a)
		(ListImpl.empty (fun (x1,f1) (x2,f2) -> if f1 > f2 then Lt else if f1 < f2 then Gt else Eq) )
		countedList		
	in
	(*Make sure q has at least 2 elements before being called*)
	let rec loop q : 'a hufftree = 
		if (ListImpl.size q = 1) then 
			fst (fst (deoption (ListImpl.remove q)) )
		else
			let t1 = deoption (ListImpl.remove q) in (* (<tree> , frequency) , queue  *)
			let t2 = deoption (ListImpl.remove (snd t1)) in (* (<tree> , frequency) , queue  *)
			let elem1 = fst t1 in (* <tree>, <frequency>*)
			let elem2 = fst t2 in (* <tree>, <frequency>*)
		loop ( ListImpl.insert ( Node(fst elem1,fst elem2) , (snd elem1)+(snd elem2) ) (snd t2) )
	in
if List.length chars = 0 then Empty
else Tree (loop queue)

let encode enc chars = 
	(*Gives the path to a specific element. Path is composed of bit*)
	let rec pathTo elem tree encoding = 
		match tree with
		| Leaf(e) -> if e = elem then Some encoding else None
		| Node (l, r) -> 
			match pathTo elem l (Zero :: encoding)  with
			| None -> 
				( match pathTo elem r (One :: encoding) with
				  | None -> None
				  | Some p -> Some p (*One :: p*)
				)
			| Some p -> Some p
	in
	let rec helper lst (encoded : bit list)= 
		match lst with
		| [] -> encoded
		| hd :: tl -> 
			match pathTo hd enc encoded with
			| None -> failwith ("Character not in tree")
			| Some p -> helper tl p		
	in
	List.rev (helper chars [])

let decode enc bits  = 
	(*Will recurse itself after finding an element*)
	let rec helper node bitLst accum = 						
	 	match node with 
		| Leaf (v) -> helper enc bitLst (v :: accum)
		| Node (t1 , t2) -> (
			match bitLst with
			| [] -> accum
			| hd :: tl -> (
				match hd with
				| Zero -> helper t1 tl accum
				| One -> helper t2 tl accum	
			)
		)				
	in
List.rev (helper enc bits []) 


