(** This module implements various utility functions on binary
    trees. The fundamental operation is [tree_fold], which is used in
    an analogous manner to [List.fold_right]. You may find {{:
    http://en.wikipedia.org/wiki/Tree_traversal} this information}
    about tree traversals helpful in implementing your solutions. *)

(** The type ['a bintree] represents a binary tree with data of type
    ['a] stored in the nodes. *)
type 'a bintree =
| Leaf
(** The [Leaf] constructor is used to signify the end of a particular
    branch of the tree. *)
| Node of 'a bintree * 'a * 'a bintree
(** The [Node] constructor represents a tree node containing a value
    of type ['a] and two subintrees. *)

(** [tree_count t] counts the number of [Node]s in the tree [t]. *)
val tree_count : 'a bintree -> int

(** [tree_sum t] computes the sum of the values in the [Nodes] of
    [t]. *)
val tree_sum : int bintree -> int

(** [tree_mem x t] returns true if and only if [x] is an element in
    [t]. *)
val tree_mem : 'a -> 'a bintree -> bool

(** [tree_preorder t] outputs a [list] containing the {{:
    http://en.wikipedia.org/wiki/Tree_traversal#Pre-order}pre-order}
    traversal of [t]. More precisely [List.nth (tree_preorder t) k] is
    the value contained [k]{^ th} visited [Node] in the pre-order
    traversal of [t]. *)
val tree_preorder : 'a bintree -> 'a list

(** The [tree_inorder] function has an analogous specification to
    [tree_preorder], but outputs the {{:
    http://en.wikipedia.org/wiki/Tree_traversal#In-order_.28symmetric.29}in-order}
    traversal of [t]. *)
val tree_inorder : 'a bintree -> 'a list

(** [tree_postorder] is similar to [tree_preorder] as well, but
    outputs a {{:
    http://en.wikipedia.org/wiki/Tree_traversal#Post-order}
    post-order} traversal. *)
val tree_postorder : 'a bintree -> 'a list

(** The [tree_fold] function generalizes fold operator to the ['a
    bintree] type. The expression [tree_fold l n t] processes the
    sub-trees of [t] via the following inductive scheme: {ul {- Each
    [Leaf] constructor returns the value [l]} {- Each [Node]
    constructor applies the function [n] to the data within the [Node]
    and its incident sub-trees.}} *)
val tree_fold : 'a -> ('b -> 'a -> 'a -> 'a) -> 'b bintree -> 'a

(** [tree_count_fold] is identical to [tree_count], but it is implemented
    with [tree_fold] *)
val tree_count_fold : 'a bintree -> int

(** [tree_sum_fold] is identical to [tree_sum], but it is implemented
    with [tree_fold] *)
val tree_sum_fold : int bintree -> int

(** [tree_mem_fold] is identical to [tree_mem], but it is implemented
    with [tree_fold] *)
val tree_mem_fold : 'a -> 'a bintree -> bool

(** [tree_preorder_fold] is identical to [tree_preorder], but it is implemented
    with [tree_fold] *)
val tree_preorder_fold : 'a bintree -> 'a list

(** [tree_inorder_fold] is identical to [tree_inorder], but it is implemented
    with [tree_fold] *)
val tree_inorder_fold : 'a bintree -> 'a list

(** [tree_postorder_fold] is identical to [tree_postorder], but it is implemented
    with [tree_fold] *)
val tree_postorder_fold : 'a bintree -> 'a list

