(* Fill in and submit this file *)
(* Name:  *)
(* NetID: *)

(** Question 3 ****************************************************************)

(**
 * [rps_round a b] accepts two symbols, [a] and [b], and determines the winner.
 * "rock" beats "scissors", "scissors" beats "paper", and "paper" beats "rock"
 * @param a b
 *     One of the choices "rock", "paper" or "scissors"
 * @return
 *     One of { -1, 0, 1, 4 }. -1 means [a] won, 1 means [b] won,
 *     0 indicates a tie.  4 indicates an invalid input.
 *)
let rps_round (a : string) (b : string) : int =
  if a = "rock" then (
    if b = "rock" then 0
    else if b = "paper" then 1
    else if b = "scissors" then -1
    else 4
  )
  else if a = "paper" then (
    if b = "rock" then -1
    else if b = "paper" then 0
    else if b = "scissors" then 1
    else 4
  )
  else if a = "scissors" then (
    if b = "rock" then 1
    else if b = "paper" then -1
    else if b = "scissors" then 0
    else 4
  )
  else 4

(* 3a *)
type move   = Rock | Paper | Scissors
type result = AWin | BWin  | Draw

let rps_round_enum (a : move) (b : move) : result =
  if a = Rock then (
    if b = Rock then Draw
    else if b = Paper then BWin
    else if b = Scissors then AWin
	else Draw
  )
  else if a = Paper then (
    if b = Rock then AWin
    else if b = Paper then Draw 
    else if b = Scissors then BWin    
	else Draw
  )
  else if a = Scissors then (
    if b = Rock then BWin
    else if b = Paper then AWin   
    else if b = Scissors then Draw
	else Draw
  ) 
	else Draw

(* 3b *)
let rps_round_nested_match (a : move) (b : move) : result =
	  match a with
	  | Rock -> 
	      (match b with
	      | Rock -> Draw      
	      | Paper -> BWin
	      | Scissors -> AWin)    
	  | Paper ->
	      (match b with
	      | Rock -> AWin
	      | Paper -> Draw 
	      | Scissors -> BWin)
	  | Scissors ->
	      (match b with
	      | Rock -> BWin
	      | Paper -> AWin
	      | Scissors -> Draw)

(* 3c *)
let rps_round_single_match (a : move) (b : move) : result =
  match (a,b) with 
  | (Rock,Rock) -> Draw
  | (Rock, Paper) -> BWin
  | (Rock, Scissors) -> AWin
  
  | (Paper, Rock) -> AWin
  | (Paper, Paper) -> Draw
  | (Paper, Scissors) -> BWin

  | (Scissors, Rock) -> BWin
  | (Scissors, Paper) -> AWin
  | (Scissors, Scissors) -> Draw

(* 3d *)
let rps_round_with_helper (a : move) (b : move) : result =
  let beats (a:move) (b:move) = 
    match (a,b) with 
      | (Rock, Scissors) -> true
      | (Scissors, Paper) -> true
      | (Paper, Rock) -> true
      | _ -> false
  in

  if beats a b 
    then AWin else 
    if beats b a 
      then BWin else
          Draw


(** exercise 4 ****************************************************************)

(* 4a *)
let all_pairs l =

  let rec helper h l2 r = 
      match l2 with
      | [] -> r
      | hd :: tl -> helper h tl ((h,hd) :: r) in
    
  
  let rec helper2 l1 l2 r =
      match l1 with 
      | [] -> r
      | hd :: tl -> helper2 tl l2 (helper hd l2 r) in
    
  List.rev (helper2 l l [])

(* 4b *)
let test_rps_eq impl1 impl2 =    
    let rec helper l = 
    match l with
    | [] -> true
    | hd :: tl -> if (impl1 (fst hd) (snd hd)) = (impl2 (fst hd) (snd hd)) then helper tl
                  else false in
    helper (all_pairs [Rock;Paper;Scissors])

(* 4c *)
let test_all_rps impls =

	let rec helper l = 
	  	match l with
	  	| [] -> true
	  	| hd :: tl -> if test_rps_eq (fst hd) (snd hd) then helper tl
	  					else false in

	helper (all_pairs impls)


(** exercise 5 ****************************************************************)

type history = move list
type player  = history -> move

let always_rock : player = fun _ -> Rock

(* 5a *)

let beats_last : player = fun history ->
  match history with
  | [] -> Rock
  | hd :: tl -> 
      (match hd with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock
      )

(* 5b *)

let always_plays move history =
  move

(* 5c *)

let rps_game (a:player) (b:player) =

    let rec game a b historyA historyB =
      let aMove = a historyB in
      let bMove = b historyA in

      match (rps_round_single_match aMove bMove) with
      | Draw -> game a b (aMove :: historyA) (bMove :: historyB)
      | AWin -> true
      | BWin -> false
    in
    game a b [] []


(** exercise 6 ****************************************************************)

(* 6a *)

let pair_filter (compare : 'a -> 'a -> 'a) (lst : 'a list) : 'a list =
  let rec helper1 l r =
      match l with
      | [] -> r
      | hd :: hd2 :: tl -> helper1 tl ((compare hd hd2) :: r)
      | hd :: tl -> helper1 tl (hd :: r) in
  List.rev (helper1 lst [])

(* 6b *)

let tournament (compare: 'a -> 'a -> 'a) (lst : 'a list) =
  let rec helper1 l = 
      match l with 
      | [] -> None
      | hd :: hd2 :: tl -> helper1 (pair_filter compare l)
      | hd :: tl -> Some (List.hd l) in

     helper1 lst

(** optional karma ************************************************************)


