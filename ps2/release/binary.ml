type bit = Zero | One

type bits = bit list

let int_to_bits (n:int) : bits = 
  let rec pow a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a) in

  let rec maxN n s = 
    if n >= pow 2 s then maxN n (s+1)
  else s-1 in 

  let rec helper1 n s r = 
    if s = -1 then r else
      if n >= (pow 2 s) then helper1 (n - (pow 2 s) ) (s-1) (One :: r)
        else helper1 n (s-1) (Zero :: r) in
  List .rev (helper1 n (maxN n 0) [])


let bits_to_int (lst:bits) : int = 
  let rec pow a n =
  match n with
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a) in

  let rec helper1 lst s r = 
    match lst with 
    | [] -> r
    | hd :: tl -> 
                  if hd = Zero then helper1 tl (s-1) r
                else helper1 tl (s-1) (r + pow 2 s)
              in
  helper1 lst ((List.length lst) -1) 0


let normalize (lst:bits) (lst':bits) : bits * bits =
  let rec pad (lst:bits) (n:int) : bits = 
    if n < 0 then failwith "pad: negative number"
    else if n = 0 then lst 
    else pad (Zero::lst) (n-1) in 
  let n,n' = List.length lst, List.length lst' in
  if n < n' then 
    (pad lst (n' - n), lst')
  else 
    (lst,pad lst' (n - n'))

let drop_leading_zeros (lst:bits) : bits = 
  int_to_bits (bits_to_int lst)

let binary_addition (lst:bits) (lst':bits) : bits = 
  let add (e1 : bit) (e2 : bit) a = 
    match (e1 , e2) with
    | (Zero , Zero) -> ( (snd a) :: (fst a), Zero ) 
    | (Zero, One) -> if (snd a) = One then ( Zero :: (fst a), One) else ( One :: (fst a) ,Zero)
    | (One, Zero) -> if (snd a) = One then ( Zero :: (fst a), One) else ( One :: (fst a) ,Zero)
    | (One, One) -> ( (snd a) :: (fst a), One)
  in
  let x = normalize lst lst' in
    let result = List.fold_right2 add (fst x) (snd x) ([], Zero) in
      drop_leading_zeros ((snd result) :: fst result )









