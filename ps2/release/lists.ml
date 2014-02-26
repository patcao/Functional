let lengths_rec  xss =
  let rec lngth lst r = 
  match lst with
  | [] -> r
  | hd :: tl -> lngth tl (r @ List.length hd :: []) in
  lngth xss []
  
let lengths_fold xss =
  let lngth e a = 
     (List.length e) :: a in

  List.fold_right lngth xss []

let lengths_lib xss = 
  List.map (List.length) xss

let find_first_value_rec  x xs = 
  let rec helper1 x xs = 
    match x with
    | [] -> None
    | hd :: tl -> if (fst hd) = xs then Some (snd hd) else
                  helper1 tl xs
  in
    helper1 x xs


let find_first_value_fold x ps =
    let find e a = 
    match e with
    | (e1, e2) -> if e1 = ps then (Some e2) else a
  in
  List.fold_right find x None

let find_first_value_lib x ps =
  let f x = (fst x) = ps in
    if (List.exists f x) then 
      Some (snd (List.find f x))
        else
        None

let rec confirm_outputs f x y =
    match f with 
    | [] -> true
    | hd :: tl -> if hd x = y then confirm_outputs tl x y
                  else false


let total_length  xss =
  List.length (List.concat xss)

let find_last_value xs x =
   let find a e= 
    match e with
    | (e1, e2) -> if e1 = x then (Some e2) else a
  in
  List.fold_left find None xs

let median  xs =
  match xs with 
    | [] -> None
    | hd :: tl -> 
      let sorted = List.sort compare xs in
        Some (List.nth sorted (( (List.length sorted) -1)/2))


