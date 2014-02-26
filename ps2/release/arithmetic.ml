type exp = Val of int | Plus of exp * exp | Times of exp*exp

let rec exp_fold v p t exp =
  match exp with
  | Val x -> v x
  | Plus (x,y) -> p (exp_fold v p t x) (exp_fold v p t y)
  | Times (x,y) -> t (exp_fold v p t x) (exp_fold v p t y)

let eval exp =
  exp_fold (fun x -> x) (fun x y -> x + y) (fun x y -> x * y) exp

let to_string exp =
  exp_fold (fun x -> string_of_int x) 
  		(fun x y -> "(" ^ x ^ "+" ^ y ^ ")")
  		(fun x y -> "(" ^ x ^ "*" ^ y ^ ")")
  		exp

