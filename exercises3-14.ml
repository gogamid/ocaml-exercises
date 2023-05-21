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
(* safe_hd and tl *)
(* quadrant *)
(* depth *)
(* shape *)
(* is_bst *)