(* Testing heaps *)

open PQueue
open Util
open Assertions

let cmp_int (a : int) (b : int) : comparison_result =
	if a < b then Lt
	else if a = b then Eq
	else Gt;;

let pq_heap = HeapImpl.empty cmp_int;;
let pq_list = ListImpl.empty cmp_int;;

(* Testing is_empty *)
TEST "test empty int" =
	(HeapImpl.is_empty pq_heap = ListImpl.is_empty pq_list)

let pq_heap = HeapImpl.insert 2 pq_heap;;
let pq_list = ListImpl.insert 2 pq_list;;

TEST "test heap 2" = 
	(HeapImpl.is_empty pq_heap = ListImpl.is_empty pq_list)
TEST "test heap 2 size" = 
	(HeapImpl.size pq_heap = ListImpl.size pq_list)
TEST "test heap 2 max" = 
	(HeapImpl.max pq_heap = ListImpl.max pq_list)

let pq_empty_heap = HeapImpl.remove pq_heap;;
let pq_empty_list = ListImpl.remove pq_list;;

TEST "remove empty" = 
	match (pq_empty_heap, pq_empty_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

let fig1_heap = HeapImpl.insert 9 (HeapImpl.insert 5 
								(HeapImpl.insert 88 (HeapImpl.insert 30 
								(HeapImpl.insert 17 (HeapImpl.insert 10 
								(HeapImpl.insert 90 (HeapImpl.insert 25 
								(HeapImpl.insert 3110 (HeapImpl.empty cmp_int)))))))));;
(*
let tree = match  (HeapImpl.remove fig1_heap) with
| Some (a,b) -> b;;*)

let fig1_list = ListImpl.insert 9 (ListImpl.insert 5 
								(ListImpl.insert 88 (ListImpl.insert 30 
								(ListImpl.insert 17 (ListImpl.insert 10 
								(ListImpl.insert 90 (ListImpl.insert 25 
								(ListImpl.insert 3110 (ListImpl.empty cmp_int)))))))));;

TEST "fig1 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 max = 3110" =
	HeapImpl.max fig1_heap = Some 3110
TEST "fig1 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 size = 9" =
	HeapImpl.size fig1_heap = 9
TEST "fig1 is_empty" = 
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem3110" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem3110 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) ->
			(v = 3110) && (HeapImpl.is_empty heap = false)

let fig1_heap = 
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem3110" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem3110 max = 90" =
	HeapImpl.max fig1_heap = Some 90
TEST "fig1 rem3110 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem3110 size = 8" =
	HeapImpl.size fig1_heap = 8
TEST "fig1 rem3110 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem3110 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem90" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem90 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 90) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem90 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem90 max = 88" = 
	HeapImpl.max fig1_heap = Some 88
TEST "fig1 rem90 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem90 size = 7" =
	HeapImpl.size fig1_heap = 7
TEST "fig1 rem90 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem90 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem88" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem88 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 88) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem88 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem88 max = 30" =
	HeapImpl.max fig1_heap = Some 30
TEST "fig1 rem88 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem88 size = 6" =
	HeapImpl.size fig1_heap = 6
TEST "fig1 rem88 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem88 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem30" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem30 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 30) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list = 
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem30 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem30 max = 25" =
	HeapImpl.max fig1_heap = Some 25
TEST "fig1 rem30 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem30 size = 5" =
	HeapImpl.size fig1_heap = 5
TEST "fig1 rem30 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem30 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem25" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem25 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 25) && (HeapImpl.is_empty heap = false)

let fig1_heap = 
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem25 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem25 max = 17" =
	HeapImpl.max fig1_heap = Some 17
TEST "fig1 rem25 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem25 size = 4" =
	HeapImpl.size fig1_heap = 4
TEST "fig1 rem25 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem25 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem17" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem17 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 17) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list = 
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem17 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem17 max = 10" =
	HeapImpl.max fig1_heap = Some 10
TEST "fig1 rem17 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem17 size = 3" =
	HeapImpl.size fig1_heap = 3
TEST "fig1 rem17 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem17 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem10" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem10 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 10) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem10 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem10 max = 9" =
	HeapImpl.max fig1_heap = Some 9
TEST "fig1 rem10 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem10 size = 2" =
	HeapImpl.size fig1_heap = 2
TEST "fig1 rem10 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem10 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem9" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem9 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 9) && (HeapImpl.is_empty heap = false)

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem9 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem9 max = 5" =
	HeapImpl.max fig1_heap = Some 5
TEST "fig1 rem9 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem9 size = 1" =
	HeapImpl.size fig1_heap = 1
TEST "fig1 rem9 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem8 is_empty = false" =
	HeapImpl.is_empty fig1_heap = false

TEST "fig1 rem5" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, _) -> false
	| (_, None) -> false
	| (Some (vh, heap), Some (vl, l)) ->
			(vh = vl) && (HeapImpl.is_empty heap = ListImpl.is_empty l)

TEST "fig1 rem5 values" =
	match HeapImpl.remove fig1_heap with
	| None -> false
	| Some (v, heap) -> (v = 5) && (HeapImpl.is_empty heap = true)

let fig1_heap = 
	match HeapImpl.remove fig1_heap with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, heap) -> heap;;

let fig1_list = 
	match ListImpl.remove fig1_list with
	| None -> failwith "error, shouldn't be empty"
	| Some (v, pair) -> pair;;

TEST "fig1 rem5 max" =
	HeapImpl.max fig1_heap = ListImpl.max fig1_list
TEST "fig1 rem5 max = None" =
	HeapImpl.max fig1_heap = None
TEST "fig1 rem5 size" =
	HeapImpl.size fig1_heap = ListImpl.size fig1_list
TEST "fig1 rem5 size = 0" =
	HeapImpl.size fig1_heap = 0
TEST "fig1 rem5 is_empty" =
	HeapImpl.is_empty fig1_heap = ListImpl.is_empty fig1_list
TEST "fig1 rem5 is_empty = true" =
	HeapImpl.is_empty fig1_heap = true

TEST "fig1 remEmpty" =
	match (HeapImpl.remove fig1_heap, ListImpl.remove fig1_list) with
	| (None, None) -> true
	| (_, _) -> false

TEST "fig1 remEmpty" =
	match HeapImpl.remove fig1_heap with
	| None -> true
	| _ -> false

let fig1_list =
	match ListImpl.remove fig1_list with
	| None -> None
	| _ -> failwith "error, should b none";;

let fig1_heap =
	match HeapImpl.remove fig1_heap with
	| None -> None
	| _ -> failwith "error, should be none";;

	

(* Tested insert in utop *)
