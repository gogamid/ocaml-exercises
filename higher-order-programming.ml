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
