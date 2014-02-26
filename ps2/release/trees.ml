type 'a bintree = Leaf | Node of 'a bintree * 'a * 'a bintree

let tree_count xs =
  let rec count tree r =
    match tree with
    | Leaf -> r 
    | Node (left,v,right) ->  (r+1) + count left 0 + count right 0 in
  count xs 0


let tree_sum xs =
  let rec tsum xs r= 
    match xs with    
    | Leaf -> 0
    | Node (left,v,right) -> (tsum left 0) + (tsum right 0) + v in 
  tsum xs 0

let tree_mem x xs =
  let rec search x xs = 
  match xs with 
  | Leaf -> false
  | Node (left,v,right) -> if ( v = x || search x left || search x right) then true else false in
search x xs

let tree_preorder xs  =
  let rec preOrder tree = 
    match tree with
    | Leaf -> []
    | Node (left,v,right) -> v :: (preOrder left ) @ (preOrder right)
  in
    preOrder xs

let tree_inorder xs =
  let rec inOrder tree = 
    match tree with
    | Leaf -> []
    | Node (left,v,right) -> (inOrder left) @ v :: (inOrder right) in
  inOrder xs

let tree_postorder xs =
  let rec postOrder tree = 
    match tree with
    | Leaf -> []
    | Node (left,v,right) -> (postOrder left) @ (postOrder right) @ v ::[] in
  postOrder xs


let rec tree_fold a f t = 
  match t with
  | Leaf -> a 
  | Node (left,v,right) -> f v (tree_fold a f left) (tree_fold a f right)


let tree_count_fold xs =   
  let f v left right = 
      1 + left + right
  in 
  tree_fold 0 f xs


let tree_sum_fold xs = 
  let f v left right = 
      v + left + right
  in
  tree_fold 0 f xs

let tree_mem_fold x xs = 
  let f v left right = 
      if v = x || left || right then true else false
  in
  tree_fold false f xs

let tree_preorder_fold xs =   
  let f v left right = 
      v :: left @ right
  in
  tree_fold [] f xs

let tree_inorder_fold xs = 
  let f v left right = 
      left @ v :: right
  in
  tree_fold [] f xs

let tree_postorder_fold xs = 
  let f v left right = 
      left @ right @ v :: []
  in
  tree_fold [] f xs
