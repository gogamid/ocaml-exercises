(* values *)
let a = 7 * (1 + 2 + 3);;
let b = "CS " ^ string_of_int 3110;;

(* operators *)
let a1 = 42 * 10;;
let b1 = 3.14 /. 2.;;
let c1 = 4.2 ** 2.;;

(* equality *)
let a2 = 42 = 42
let b2 = "hi" = "hi"  (* structural *)
let c2 = "hi" == "hi"  (* physical: check for the address *)

(* assert *)
let a3 = assert true;;
(* let b3 = assert false;; *)
let c3 = assert (2110 <> 3110);;
let c31 = assert (not (2110 = 3110));;

(* if *)
let a4 = if 2 > 1 then 42 else 7

(* double fun *)
let double x = x * 2;;
let a5 = assert (double 2 = 4)
let b5 = assert (double (-4) = -8)

(* more fun *)
let cube x = x ** 3.;;
let sign x = if x > 0 then 1 else if x<0 then -1  else 0;;
let area r = Float.pi *. r ** 2.;;

(* for comparing floats because floating-point arithmetic is not exact.*)
let close_enough a b =
  Float.abs (a -. b) < 1e-5

let a6 = assert (close_enough (area 1.) (Float.pi))

(* RMS *)
let rms x y = sqrt((x *. x +. y *. y)/. 2.);;
let _ = assert (close_enough (rms 2. 2.) 2.)
let _ = assert (close_enough (rms 7. 42.) 30.10813)

(* date fun *)
let date d m =
  if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct" || m = "Dec"
    then d >= 1 && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
    then d>=1 && d<=30
  else if m = "Feb"
    then d>=1 && d<=28
  else false;;

 let _ = assert ((date 31 "Feb") = false)
 let _ = assert ((date 28 "Feb") = true)
 let _ = assert ((date 31 "Jan") = true)

(* fib *)
let rec fib1 n prev cur =
  if n = 1 then cur
  else fib1 (n-1) cur (prev+cur);;
let fib n = fib1 n 0 1;;

let _ = assert (fib 3 = 2)
let _ = assert (fib 5 = 5)
let _ = assert (fib 6 = 8)

(* poly types *)
let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y

(* divide *)
let divide ~numerator:x ~denominator:y = x /. y

(* associativity *)
let add x y = x + y;;
let _ = add 5 1
let _ = add 5
let _ = (add 5) 1
(* let _ = add (5 1) *)

(* average *)
let (+/.) x y = (x +. y) /. 2.;;
