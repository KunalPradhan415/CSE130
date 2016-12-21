(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)
(* sqsum : int list -> int
 * takes a list of integers [x1;...;xn]) 
 * and returns the integer: x1^2 + ... + xn^2
 * eg sqsum [-1;-2;-3;-4] -> 30
 *)
let sqsum xs =
  (* write folding_fn *) 
  let f a x = a + (x * x) in
  (*write base *)
  let base = 0 in
    List.fold_left f base xs
  ;;

(* pipe : ('a -> 'a) list -> ('a -> 'a) . 
 * The function pipe takes a list of functions [f1;...;fn]) and returns a 
 * function f such that for any x, the application f x returns the result fn(...(f2(f1 x)))  
 *)
let pipe fs = 
  let f a x = fun curry -> x ( a curry) in
  let base = fun x -> x in
    List.fold_left f base fs
  ;;

(* sepConcat : string -> string list -> string
 * sepConcat is a curried function which takes as input a string sep
 * to be used as a separator, and a list of strings [s1;...;sn]. 
 * If there are 0 strings in the list, then sepConcat should return "". 
 * If there is 1 string in the list, then sepConcat should return s1. 
 * Otherwise, sepConcat should return the concatination s1 sep s2 sep s3 ... sep sn.
 * eg sepConcat "X" ["hello"] -> "hello"  
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x =  a^sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l
;;

(* stringOfList : ('a -> string) -> 'a list -> string. 
 * The first input is a function f: 'a -> string which will be called by
 * stringOfList to convert each element of the list to a string. 
 * The second input is a list l: 'a list, which we will think of as 
 * having the elemtns l1, l2, ..., ln. 
 * It will return a string representation of the list l as a concatenation
 * of the following: "[" (f l1) "; " (f l2) "; " (f l3) "; " ... "; " (f ln) "]"
 * I used "; " as sep string and concatenated brackets as needed
 *)
let stringOfList f l = ( "[" ^ sepConcat "; " ( List.map f l ) ^ "]" )
;;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)
(* clone : 'a -> int -> 'a list which first takes as 
 * input x and then takes as input an integer n. 
 * The result is a list of length n, where each element is x
 * eg clone "foo" 2 -> ["foo"; "foo"] 
 *)
let rec clone x n = 
  (*check if the length of the closed list is at least 1*)
  if n <= 0 then []
  (* then call recursively *)
  else x::(clone x (n-1))
;;
(* padZero : int list -> int list -> int list * int list
 * take two lists: [x1,...,xn] [y1,...,ym] and adds zeros 
 * in front to make the lists equal
 * eg padZero [9;9] [1;0;0;2] -> ([0;0;9;9],[1;0;0;2])  
 *)
let rec padZero l1 l2 =
(*if lists are same length- no need to pad *) 
if (List.length l1) = (List.length l2) then (l1,l2) 
else 
  if (List.length l1) <  (List.length l2) then padZero (0::l1) l2 
  else padZero l1 (0::l2)    
;;

(* removeZero : int list -> int list
 * takes a list and removes a prefix of trailing zeros.
 * eg removeZero [0;9;9] -> [9;9]
*) 
let rec removeZero l = 
   (* make sure that l is of length greater than 0 before removing zero prefix recursively*) 
   match l with
   | (h::t) -> if h = 0 then removeZero t else l
   | [] -> []    
;;

(*  bigAdd : int list -> int list -> int list
 * takes two integer lists, where each integer is in the range [0..9] and
 * returns the list corresponding to the addition of the two big integers
 * eg bigAdd [9;9] [1;0;0;2] = [1;1;0;1] 
 *)

let bigAdd l1 l2 = 
  let add (l1, l2) = 
  (* take carry into account and see if a carry is generated *)
    let f (carry, l) (x,y) =
    (* get intermediate value *) 
      let v =  carry + x + y in
      ( (if v > 9 then 1 else 0 ), ( v mod 10 ) :: l ) 
    in
    (* start with no carry and empty list to return *)
    let base = (0, []) in
    let args = List.rev ( List.combine l1 l2) @ [(0,0)]  in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))
  ;;

 (* mulByDigit : int -> int list -> int list
  * takes an integer digit and a big integer, and
  * returns the big integer list which is the result of multiplying the big integer with the digit
  * eg mulByDigit 9 [9;9;9;9] = [8;9;9;9;1]
  *) 

let rec mulByDigit i l = 
  if i = 0 then [] else ( bigAdd (mulByDigit (i-1)  l) l ) 
;;

(* bigMul : int list -> int list -> int list
 * takes two big integers and multiplies them together
 * eg bigMul [9;9;9;9] [9;9;9;9] = [9;9;9;8;0;0;0;1]
 *)
let bigMul l1 l2 = 
  let f (level, sum) x =
  (*every time you move left across the second list, add a zero *) 
  let newLevel = ( level @ [0] ) in   
  ( newLevel, bigAdd sum ((mulByDigit x l1) @ level) )  
  in
  (* level and sum both start at zero*)
  let base = ([],[]) in
  let args = List.rev l2 in
  let (_, res) = List.fold_left f base args in
    res
  ;;