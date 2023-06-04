let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double
let fourth = twice square

let ( $ ) f x = f x
(*applies function to value, fyi + op first then $*)

let ( @@ ) f g x = x |> g |> f
let exampleAt = (String.length @@ string_of_int) 100

let rec repeat f n x =
    match n with
    | 0 -> x
    | _ -> repeat f (n-1) (f x)

let product_left l =
    List.fold_left ( *. ) 1.0 l

let product_right l =
    List.fold_right ( *. ) l 1.0

let product_left1  = List.fold_left ( *. ) 1.0

let product_right1 = ListLabels.fold_right ~f:( *. ) ~init:1.0

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let sum_cube_odd n =
    0 -- n
    |> List.filter (fun x -> x mod 2 <> 0)
    |> List.map (fun x -> x * x * x)
    |> List.fold_left ( + ) 0

let rec exists_rec p = function
    [] -> false
    |  x :: xs -> p x || exists_rec p xs

let exists_fold p = List.fold_left (fun b x -> b || p x) false

let exists_lib = List.exists

let ac_balance1 balance debits =
    balance - List.fold_left ( + ) 0 debits

let ac_balance2 balance debits =
    balance - List.fold_right ( + ) debits 0

let rec ac_balance3 balance = function
    [] -> balance
    | d :: ds -> ac_balance3 (balance - d) ds;;

let uncurried_append (l1, l2) = List.append l1 l2
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (a, b) = Stdlib.max a b

(* map composition *)
let map_comp f g lst = List.map f (List.map g lst)
let map_comp1 f g lst = List.map ( fun x -> f (g x) ) lst

(* more list fun *)
let greater_three = List.filter (fun x -> (String.length x) > 3)
let addOneFloat = List.map (fun x -> x +. 1.0)
let joinString sep =
    List.fold_left (fun acc el -> if acc = "" then acc ^ el else acc ^ sep ^ el ) ""


(* association list keys *)
let d = [("rectangle", 4); ("nonagon", 9); ("icosagon", 20); ("nonagon", 9);]
let keys l = l |> List.rev_map fst |> List.sort_uniq Stdlib.compare

(* valid matrix *)
 let is_valid_matrix = function
    | [] -> false
    | r :: rows ->
        let col_len = List.length r in
        col_len > 0 && List.for_all(fun r' -> col_len = List.length r') rows



(* row vector add*)
let add_row_vectors = List.map2 ( + ) ;;

(* matrix add *)
let add_matrices = List.map2 add_row_vectors

(* matrix multiply *)
