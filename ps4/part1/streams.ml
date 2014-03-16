type 'a stream = Stream of 'a * (unit -> 'a stream)

let id           = fun x -> x
let fork (f,g) x = f x, g x
let rec take n (Stream (h,t)) = if n=0 then [] else h::take (n-1) (t ())

(**val unfold : ('a -> 'b * 'a) -> 'a -> 'b stream**)
let rec unfold (f : 'a -> 'b * 'a) (s : 'a) = 
	match (f s) with
	| b,a -> Stream (b, (fun () -> unfold f a))

let univ (f,g) x = (fun x -> unfold (fork x)) (f,g) x

let hd (Stream (h,_)) = h
let tl (Stream (_,t)) = t ()

let repeat   x = univ ( (fun x -> x), (fun x -> x) ) x

let map      f = univ ( (fun (s : 'a stream) -> f (hd s) ), (fun (s : 'a stream) -> tl s) ) 

let diag     (s : 'a stream stream) = univ ( 
						(
							fun (s1 : 'a stream stream) -> hd (hd s1)), 
							(** Generate next seed **)
							(fun (s1 : 'a stream stream) -> tl (map (fun x -> tl x) s1) 
						)
					 ) s

let suffixes s = univ ( 
						(fun (seed :'a stream) -> seed), (** Generate element from seed**)							
						(** Generate next seed **)
						(fun (Stream(e,f) : 'a stream) -> f ()) ) s

let interleave s s' = univ  (
								(fun (s1,s2,count) -> 
									if count mod 2 = 0 then hd s1
										else hd s2
								)
								,
								(** Generate next seed **)
								(fun (s1,s2,count) ->
									if count mod 2 = 0 then
										 (tl s1), s2 , 1
									else
										s1 ,(tl s2), 0
								)								
							) 
								(s,s',0)

let piFormula n = (4.0 *. (-1.0) ** n) /. (2.0 *. n +. 1.0)
let fibs         = univ ( (fun (s1,s2,s3) -> s2) , (fun (s1,s2,s3) -> (s2,s3,s2+s3)) ) (0,0,1)
let pi           = univ ( (fun (sum,n) -> sum), (fun (sum,n) -> (sum +. piFormula (n+.1.0) ,n +. 1.0) )) 
					(4.0,0.0)


(**Counts the frequency of occurence of each digit returns a list of the counted digits**)
let countList numLst = 
	let rec helper lst accum (count,curr) =
		match lst with
		| [] -> curr :: count :: accum
		| hd :: tl -> if hd = curr then helper tl accum (count+1,curr)
						else helper tl (curr :: count:: accum) (1,hd)
	in
	List.rev (helper numLst [] (0,List.hd numLst))
	
let look_and_say = univ ((fun x -> x),(fun x -> countList x )) [1]







(** Creates a list that contains each of the digits of the number seperated **)
let intToList n = 
	let rec helper n lst = 
		if n > 0 then
			let ones = n mod 10 in
			helper ((n-ones) / 10) (ones :: lst)
		else lst
	in
	helper n []

(** Converts a list of digits into an int **)
let listToInt lst : int = 
	let rec helper n lst = 
		match lst with
		| [] -> n
		| hd :: tl -> helper (10*n + hd) tl
	in
	helper 0 lst
