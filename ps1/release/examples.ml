(*
 * This file contains all of the examples from the writeup as unit tests.  You
 * can execute them by running 
 *
 *  $ cs3110 compile examples.ml"
 *  $ cs3110 test examples.ml"
 *
 * You can also load them into the toplevel, but it requires a bit of work:
 *
 *  $ utop -I _build
 *
 *  # #require "pa_ounit";;
 *  # #require "pa_ounit.syntax";;
 *  # #load "ps1.d.cmo";;
 *  # #use "examples.ml";;
 *
 * you are encouraged to write your own tests as well.
 *)
open Ps1
open Assertions

TEST "example_3a_1" =
  rps_round_enum Rock Paper = BWin

TEST "example_3a_2" =
  rps_round_enum Scissors Paper = AWin

TEST "example_3a_3" =
  rps_round_enum Paper Paper = Draw



TEST "example_3b_1" =
  rps_round_nested_match Rock Paper = BWin

TEST "example_3b_2" =
  rps_round_nested_match Scissors Paper = AWin

TEST "example_3b_3" =
  rps_round_nested_match Paper Paper = Draw



TEST "example_3c_1" =
  rps_round_single_match Rock Paper = BWin

TEST "example_3c_2" =
  rps_round_single_match Scissors Paper = AWin

TEST "example_3c_3" =
  rps_round_single_match Paper Paper = Draw



TEST "example_3d_1" =
  rps_round_with_helper Rock Paper = BWin

TEST "example_3d_2" =
  rps_round_with_helper Scissors Paper = AWin

TEST "example_3d_3" =
  rps_round_with_helper Paper Paper = Draw



TEST "rps_round_enum" =
  rps_round_enum Rock Rock = Draw

(* 4a Tests *) 
TEST "example_4a_1" =
  List.sort compare (all_pairs ['a'; 'b']) =
  List.sort compare [('a', 'a'); ('a', 'b'); ('b', 'a'); ('b', 'b')]

TEST "example_4a_2" =
  List.sort compare (all_pairs [0;1;2]) =
  List.sort compare [(0, 0); (0, 1); (0, 2);
                     (1, 0); (1, 1); (1, 2);
                     (2, 0); (2, 1); (2, 2);
                    ]

(* 4b Tests *)
TEST "example_4b_1" =
  test_rps_eq rps_round_enum rps_round_enum

TEST "example_4b_2" =
  test_rps_eq rps_round_enum rps_round_with_helper

TEST "example_4b_3" = 
  let f (a:move) (b:move) = Draw in
  not(test_rps_eq rps_round_enum f)


TEST "example_4b_3" =
  let rps_round_bogus a b = AWin in
  not (test_rps_eq rps_round_enum rps_round_bogus)

TEST "example_4b_4" =
  let rps_round_bogus a b = AWin in
  test_rps_eq rps_round_bogus rps_round_bogus

(* 4c Tests *)
TEST "example_4c_1" =
    test_all_rps [rps_round_enum; rps_round_single_match;
    rps_round_nested_match; rps_round_with_helper]

TEST "example_4c_2" =
  let rps_round_bogus a b = AWin in
  not (test_all_rps [rps_round_enum; rps_round_bogus])

(* 5a Tests *)
TEST "example_5a_1" =
  beats_last [Rock; Paper; Paper; Scissors] = Paper
TEST "example_5a_2" =
  beats_last [Paper] = Scissors
TEST "example_5a_3" =
  beats_last [] = Rock
TEST "example_5a_1" =
  beats_last [Paper; Rock; Paper; Paper;Rock] = Scissors
TEST "example_5a_1" =
  beats_last [Scissors; Paper; Paper; Scissors; Rock] = Rock

(* 5b Tests *)
let always_paper =
  always_plays Paper

TEST "example_5b_1" =
  always_paper [] = Paper

TEST "example_5b_2" =
  always_paper [Scissors; Scissors; Scissors] = Paper


let always_Scissors = 
  always_plays Scissors

TEST "example_5b_3" =
  always_Scissors [] = Scissors

TEST "example_5b_4" =
  always_Scissors [Scissors; Rock ; Paper] = Scissors


  let always_Rock = 
  always_plays Rock

TEST "example_5b_5" =
  always_Rock [] = Rock

TEST "example_5b_6" =
  always_Rock [Scissors; Paper; Scissors] = Rock

TEST "example_5c" =
  rps_game (always_plays Rock) (always_plays Paper) = false

(* 6a Tests *)
TEST "example_6a_1" =
  pair_filter max [0; 5; 7; 2; 3; -10] = [5; 7; 3]

TEST "example_6a_2" =
  pair_filter min [1; 2; 3; 4; 5] = [1; 3; 5]



(* 6b Tests *) 
TEST "example_6b_1" = 
  tournament max [1; 4; 3; 2; 7; 9; 0] = Some 9

TEST "example_6b_2" =
  tournament min [] = None

TEST "example_6b_3" =
  tournament min [1;8;0;5;7] = Some 0

