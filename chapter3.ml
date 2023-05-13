(* list expressions *)
let _ = [1;2;3;4;5]
let _ = 1 :: 2 :: 3 :: 4 :: 5 :: []
let _ = [1] @ [2;3;4] @ [5]

(* product *)
let rec product = function
  | [] -> 1
  | h :: t -> h * product t

let _ = assert (product [1;2;3;4;5] = 120)
(* concat *)
let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let _ = assert (concat ["a";"b";"c";"d";"e"] = "abcde")

(* patterns *)
let isFirstBigred = function
  | "bigred" :: _ -> true
  | _ -> false

let _ = assert (isFirstBigred ["bigred";"foo";"bar"] = true)

let isTwoOrFourEls = function
  | [_;_] | [_;_;_;_] -> true
  | _ -> false

let _ = assert (isTwoOrFourEls [1;2;3;4] = true)

let isFirstTwoElsEqual = function
  | h1 :: h2 :: _ -> h1 = h2
  | _ -> false

let _ = assert (isFirstTwoElsEqual [1;1;2;3;4] = true)

let get5thEl (l: int list) : int =
  if List.length l > 5 then List.nth l 4 else 0

let _ = assert (get5thEl [1;2;3;4;5;6;7;8;9;10] = 5)

let sortIntList (l: int list) : int list =
  List.sort compare l

let _ = assert (sortIntList [1;3;2;5;4] = [1;2;3;4;5])

let sortIntListDesc (l: int list) : int list =
  List.sort compare l |> List.rev

let _ = assert (sortIntListDesc [1;3;2;5;4] = [5;4;3;2;1])

let getLastEl (l: 'a list) : 'a =
  l |> List.rev |> List.hd

let _ = assert (getLastEl [1;2;3;4;5] = 5)

let containsZero (l: int list) : bool =
  List.exists (fun x -> x = 0) l

let _ = assert (containsZero [1;2;3;4;5] = false)

