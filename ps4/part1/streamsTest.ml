open Assertions
open Streams

(** Takes a stream and creates a list containing the first 10 elements **)
(**How many elements it creates can be changed by changing input to helper **)
let makeList (st : 'a stream) : 'a list = 
	let rec helper st lst count = 
		if count > 0 then
			 helper (tl st) ( (hd st):: lst) (count - 1)
		else
			lst
	in
	List.rev (helper st [] 10);;

TEST "Test Unfold" =
	let str = unfold (fun x -> (x,x+1)) 0 in
	makeList str = [0;1;2;3;4;5;6;7;8;9]

TEST "Test Repeat" = 
	let str = repeat 'x' in
	makeList str = ['x';'x';'x';'x';'x';'x';'x';'x';'x';'x']

TEST "Test Map" = 
	let natNumbers = unfold (fun x -> (x,x+1)) 0 in
	let mapped = map (fun x -> x + 1) natNumbers in
	makeList mapped = [1;2;3;4;5;6;7;8;9;10]

(** Generates a stream that has a 1 at the specified location and 0 everywhere else **)
(** Example: If n = 2  [0;0;1;0;0;0;.....] **)
let genDiagStream n = 
	univ ( (fun count -> if count = n then 1 else 0), (fun count -> count + 1) ) 0

TEST "Test Diag" = 
	let streamStream = univ ( (fun count -> genDiagStream count),( fun count -> count + 1) ) 0 in
	makeList (diag streamStream) = [1;1;1;1;1;1;1;1;1;1]


TEST "Test Suffixes" = 
	let s1 = unfold (fun x -> (x,x+1)) 0 in
	let suf = suffixes s1 in 
	let rec checker numReps count suffx = 
		if numReps > 0 then
			if makeList (hd suffx) = makeList (unfold (fun x -> (x,x+1)) count ) then
				checker (numReps - 1) (count + 1) (tl suffx)
			else
				false
		else 
			true
	in
	checker 15 0 suf

TEST "Test Interleaves" =
	let s1 = unfold (fun x -> (x,x+2)) 0 in
	let s2 = unfold (fun x -> (x,x+2)) 1  in
	makeList (interleave s1 s2) = [0;1;2;3;4;5;6;7;8;9]

TEST "Test Fib" = makeList fibs = [0;1;1;2;3;5;8;13;21;34]

TEST "Test pi" = makeList pi = [4.; 2.66666666666666696; 3.46666666666666679; 2.89523809523809561;3.33968253968254025; 
2.97604617604617649; 3.28373848373848443; 3.01707181707181782;3.25236593471887669; 3.04183961892940324]

TEST "Test look_say" = makeList look_and_say = [[1]; [1; 1]; [2; 1]; [1; 2; 1; 1]; [1; 1; 1; 2; 2; 1]; [3; 1; 2; 2; 1; 1];              [1; 3; 1; 1; 2; 2; 2; 1]; [1; 1; 1; 3; 2; 1; 3; 2; 1; 1];                              
 [3; 1; 1; 3; 1; 2; 1; 1; 1; 3; 1; 2; 2; 1];
 [1; 3; 2; 1; 1; 3; 1; 1; 1; 2; 3; 1; 1; 3; 1; 1; 2; 2; 1; 1]]


