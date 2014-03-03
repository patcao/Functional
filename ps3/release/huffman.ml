
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
	let rec pathTo elem tree path = 
		match tree with
		| Leaf(e) -> if e = elem then Some [] else None
		| Node (l, r) -> 
			match pathTo elem l (Zero::path) with
			| None -> 
				( match pathTo elem r (One :: path) with
				  | None -> None
				  | Some p -> Some (One :: p)
				)
			| Some p -> Some (Zero :: p)
	in
	let rec helper lst code = 
		match lst with
		| [] -> code
		| hd :: tl -> 
			(pathTo hd enc []) @ (helper tl code)
	in
	helper chars []

let decode enc bits  = failwith "sudo make me a sandwich"

