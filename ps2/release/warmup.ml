let sum xs =
  List.fold_left (+) 0 xs

let rev xs =
  List.fold_left  (fun a e -> e :: a) [] xs

let max2 xs =
  match xs with
  | hd :: hd2 :: tl -> 
      let m = List.fold_left max hd xs in
        let rm a e = 
          if e < m then e :: a
          else a
        in

      let lst = List.fold_left rm [] xs in
      List.fold_left max (List.hd lst) lst
  | _ -> failwith "Less than 2 elements"


let max3 xs = 
  let sndMax a e = 
    match a with
    | (None, None) -> (None, Some e)
    | (None, y) -> (y, Some e)
    | (Some x, Some y) -> if e > y then (Some y, Some e) else
                            if e > x then (Some e, Some y) else
                                (Some x, Some y)
    | (Some _, None) -> failwith "bad"
  in 
  let pair = List.fold_left sndMax (None,None) xs in
  match pair with
  | (None , _ ) -> failwith "Less than 2 elements"
  | (Some x, _ ) -> x



let all_pairs xs =
  failwith "Have you seen my hat?
            No. I have not seen any hats
            around here.
            OK. Thank you anyway."
