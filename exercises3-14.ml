(* take not tail recursive *)
(* let rec take n l =
    if n == 0 then []
    else match l with
    | [] -> []
    | x :: xs -> x :: take (n-1) xs *)

(* take drop tail recursive *)
let take n l =
 let rec inner n l acc =
    match l with
    | [] -> List.rev acc
    | h::t -> if n = 0 then List.rev acc else inner (n-1) t (h::acc)
 in
 inner n l []

let rec from i j l =
  if i > j then l
  else from i (j - 1) (j :: l)

let (--) i j =
  from i j []

let longList = 0 -- 1_000_000
(* let _ = assert (take 1_000_000 longList = List.init 1_000_000 (fun x -> x)) *)
let _ = assert (take 1_000_000 longList = List.init 1_000_000 (fun x -> x))
let _ = assert (take 0 longList = [])
let _ = assert (take 1 longList = [0])
let _ = assert (take 2 [0] = [0])

(* drop already in tail recursive because after rec funtion there is no need for calculation*)
let rec drop n l =
   if n == 0 then l
   else match l with
   | [] -> []
   | x :: xs -> drop (n-1) xs

let _ = assert (List.length longList = 1_000_001)
let _ = assert (drop 0 longList = longList)
let _ = assert (drop 1 longList = 1 -- 1_000_000)
let _ = assert (drop 3 [1;2;3]= [])

(* unimodal *)
let rec isMonDec = function
    | [] | [_] -> true
    | h1 :: (h2 :: t2 as t) ->
        h1 >= h2 && isMonDec t

let rec isMonIncThenDec = function
    | [] | [_] -> true
    | h1 :: (h2 :: t2 as t) as l ->
        if h1<=h2 then isMonIncThenDec t else isMonDec l

let isUniModal l =
    isMonIncThenDec l


let _ = assert (isUniModal [1;2;3;4;5;4;3;2;1])
let _ = assert (isUniModal [1;2;3;4;5;6;7;8;9])

(* safe_hd and tl *)
let safe_hd = function
    | [] -> None
    | h :: _ -> Some h

let _ = assert (safe_hd [1;2;3] = Some 1)
let _ = assert (safe_hd [] = None)

let safe_tl = function
    | [] -> None
    | _ :: t -> Some t

let _ = assert (safe_tl [1;2;3] = Some [2;3])
let _ = assert (safe_tl [] = None)

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign =
    if x < 0 then Neg
    else if x = 0 then Zero
    else Pos

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None

let _ = assert (quadrant (1,1) = Some I)
let _ = assert (quadrant (-1,1) = Some II)
let _ = assert (quadrant (-1,-1) = Some III)
let _ = assert (quadrant (1,-1) = Some IV)
let _ = assert (quadrant (0,0) = None)

(* depth *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
let depth t =
    let rec inner t d =
        match t with
        | Leaf -> d
        | Node (l, _, r) -> max (inner l (d+1)) (inner r (d+1))
    in
    inner t 0

let _ = assert (depth (Node (Leaf, 1, Leaf)) = 1)
let _ = assert (depth (Node (Node (Leaf, 1, Leaf), 2, Leaf)) = 2)

(* shape *)
let rec shape t1 t2 =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Node (l1, _, r1), Node (l2, _, r2) -> shape l1 l2 && shape r1 r2
    | _ -> false

let _ = assert (shape (Node (Leaf, 1, Leaf)) (Node (Leaf, 2, Leaf)))
let _ = assert (not (shape (Node (Leaf, 1, Leaf)) (Node (Node (Leaf, 2, Leaf), 1, Leaf))))
