type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree


let rec size = function
  | Leaf -> 0
  | Node(_,l,r) -> 1 + size l + size r

let rec depth = function
  | Leaf -> 0
  | Node(_,l,r) -> 1 + max (depth l) (depth r)

let rec same_shape t1 t2 =
  match t1,t2 with
  | Leaf, Leaf -> true
  | Node(_, l1, r1), Node(_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _ -> false

(*TODO: Implement later*)
let is_bst: ('a*'b) tree = function
  | _ -> true

let t1 =
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(6, Leaf, Leaf),
      Node(7, Leaf, Leaf)
    )
  )
let t =
  Node(4,
    Node(2,
      Node(1, Leaf, Leaf),
      Node(3, Leaf, Leaf)
    ),
    Node(5,
      Node(6, Leaf, Leaf),
(*      Node(7, Leaf, Leaf) *)
      Leaf
    )
  )

let tSize = size t
let tDepth = depth t
let isSame = same_shape t t1