
open Assertions


TEST "test" =    
		1 = 2
   (* HeapImpl.traverse (HeapImpl.empty (HeapImpl.empty (fun x y -> if x > y then Gt else if x < y then Lt else Eq))) = [2;1]


    Let pq = HeapImpl.insert 2 (HeapImpl.insert 1 
    	(HeapImpl.empty (fun x y -> if x > y then Gt else if x < y then Lt else Eq))) in*)