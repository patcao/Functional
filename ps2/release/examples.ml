
(* Folding on lists ***********************************************************)

open Warmup
(* 1a *)
TEST  "example_1a" = 
  sum [1;2;3;4;5;-5;6;7;8;9;10] = 50
(* 1b *)
TEST  "example_1b" = 
  rev ["a";"b";"c";"d"] = ["d";"c";"b";"a"]
(* 1c *)
TEST "example_1c_1" = max2 [0; 1; 4; -13] = 1
TEST "example_1c_2" = max2 [1.; 3.5; 3.5; 5.] = 3.5
TEST "example_1c_3" = max2 ["one"; "two"; "three"] = "three"

TEST "example_1c_4" = try ignore(max2 ["whoops"; "whoops"]); false with
  | _ -> true

TEST "example_1c_5" = max2 [5; 5; 1; 2] = 2

open Lists

(* 2a *)
TEST "example_2a_1" =
  lengths_rec [["zar"; "doz"]; []; ["ocaml"; "rocks"; "a" ; "b"]] = [2; 0; 4]

TEST "example_2a_2" =
  lengths_fold [["zar"; "doz"]; []; ["ocaml"; "rocks";"a" ; "b"]] = [2; 0; 4]

TEST "example_2a_3" =
  lengths_lib [["zar"; "doz"]; []; ["ocaml"; "rocks";"a" ; "b"]] = [2; 0; 4]


(* 2b *)
TEST "example_2b_1" =
  find_first_value_rec [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'x' = Some 2

TEST "example_2b_2" =
  find_first_value_rec [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'a' = None

TEST "example_2b_3" =
  find_first_value_fold [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'y' = Some 4

TEST "example_2b_4" =
  find_first_value_fold [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'w' = None

TEST "example_2b_5" =
  find_first_value_lib [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'z' = Some 8

TEST "example_2b_6" =
  find_first_value_lib [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'w' = None

(* 3a *)

TEST "example_3a_1" = confirm_outputs [(+) 1; (+) 1; fun  x -> 2] 1 2 = true
TEST "example_3a_2" = confirm_outputs [(+) 1; (+) 2; (+) 1] 1 2 = false

(* 3b *)

TEST "example_3b_1" = total_length [[]; [17;2];[5]] = 3
TEST "example_3b_2" = total_length [[7;8;9]; [17;2];[5]] = 6
TEST "example_3b_3" = total_length [[];[];[]] = 0

(* 3c *)

TEST "example_3c_1" =
  find_last_value [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'x' = Some 12

TEST "example_3c_2" =
  find_last_value [('x', 2); ('y', 4); ('z', 8); ('x', 12)] 'w' = None

TEST "example_3c_3" =
  find_last_value [('x', 2); ('y', 4); ('z', 8); ('x', 12); ('x',15)] 'x' = Some 15

(* 3d *)

TEST "example_3d_1" = median [3;6;9;12;0;2]  = Some 3
TEST "exapmle_3d_2" = median [3;6;12;-4;0;2] = Some 2
TEST "example_3d_3" = median [] = None
TEST "example_3d_4" = median [3;6;9;12;0;2;15;1]  = Some 3

open Binary

(* 4 *)

TEST "example_4_1" = int_to_bits 0 = []
TEST "example_4_2" = int_to_bits 1 = [One]
TEST "example_4_3" = int_to_bits 2 = [One;Zero]
TEST "example_4_4" = int_to_bits 42 = [One;Zero;One;Zero;One;Zero]
TEST "example 4_5" = bits_to_int [Zero;One;Zero;One;Zero] = 10 
TEST "example 4_6" = bits_to_int [One;One;One;One;One;One;One] = 127
TEST "example 4_6a" = bits_to_int [Zero;Zero;Zero;One;One;One;One;One;One;One] = 127
TEST "example_4_7" = binary_addition [Zero] [Zero;Zero] = []
TEST "example_4_8" = binary_addition [Zero] [One] = [One]
TEST "example_4_9" = binary_addition [Zero;Zero;One] [Zero;Zero;One] = [One;Zero]
TEST "example_4_10" = binary_addition [One;One] [One] = [One; Zero; Zero]
TEST "example_4_11" = binary_addition [Zero;One;One] [One;One] = [One; One; Zero]
TEST "example_4_12" = binary_addition [One;One;One] [Zero] = [One; One; One]
TEST "example_4_13" = binary_addition [Zero;One;One;One] [One] = [One;Zero;Zero;Zero]
TEST "example_4_14" = binary_addition [Zero;Zero;One;One;One] [Zero;One] = [One; Zero; Zero; Zero]

(* Folding on trees ***********************************************************)

open Trees


let example_tree =
  Node (
    Node (
      Node (Leaf, 2, Leaf),
      4,
      Node (Leaf,
            7,
            Node (Leaf, 8, Leaf))),
    3110,
    Node (
      Leaf,
      6,
      Node (Node (Leaf, -3, Leaf),
            14,
            Leaf)))

(* 5a *)

TEST "example_5a_1" = tree_sum example_tree = 3148

(* 5b *)

TEST "example_5b_1" = tree_mem 8 example_tree = true
TEST "example_5b_2" = tree_mem 8 Leaf = false
TEST "example_5b_3" = tree_mem 100 example_tree = false

(* 5c *)

TEST "example_5c_1" = tree_preorder example_tree = [3110; 4; 2; 7; 8; 6; 14; -3]

(* 5d *)

TEST "example_5c_2" = tree_inorder example_tree = [2; 4; 7; 8; 3110; 6; -3; 14]

(* 5e *)

TEST "example_5c_3" = tree_postorder example_tree = [2; 8; 7; 4; -3; 14; 6; 3110]

(* 6b *)
TEST "example_6a_1" = tree_count_fold example_tree = 8
TEST "example_6a_2" = tree_sum_fold example_tree = 3148
TEST "example_6a_3" = tree_mem_fold 8 example_tree = true
TEST "example_6a_4" = tree_mem_fold 100 example_tree = false
TEST "example_6a_5" = tree_preorder_fold example_tree = [3110; 4; 2; 7; 8; 6; 14; -3]
TEST "example_6a_6" = tree_inorder_fold example_tree = [2; 4; 7; 8; 3110; 6; -3; 14]
TEST "example_6a_7" = tree_postorder_fold example_tree = [2; 8; 7; 4; -3; 14; 6; 3110]

(* Folding on expressions *****************************************************)

open Arithmetic

let example_exp = 
  Times ( 
    Plus ( Val 1 , Val 1 ) ,
    Plus (
      Times ( Val 3 , Val 4 ) ,
      Val 1
      )
    )

(* 8a *)
TEST "example_8a_1" = eval example_exp = 26

(* 8b *)
TEST "exapmle_8b_1" = to_string example_exp = "((1+1)*((3*4)+1))"

