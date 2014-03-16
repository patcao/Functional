(** Constants **)

(**Test Binary operators**)

(**Test Unary operators**)

(**Test Var operators**)



(**Test Cons operators**)

(**Test IfThenElse operators**)

(**Test Let operators**)

(**Test LetRec operators**)

(**Test App operators**)

(**Test Match operators**)

let m = [ [1;2;3] ; [4;5;6] ; [7;8;9]] in 
let x = 5 in
let v = match m with
		| x :: tl -> (match x with 
					  hd :: tl -> hd) in
		if v = 1 then x+v else 0

