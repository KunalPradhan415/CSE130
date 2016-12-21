(* CSE 130: Programming Assignment 1
 * misc.ml
 * Owner: Kunal Pradhan, A11385800
 * Date: 9/23/2016
 *)

(*append: a list : a list -> a list 
 * used in place of @ since that
 * is not allowed
 * taken from example given in discussion
 * appends a list to another by using
 * recursion, as if the l1 is empty- base case,
 * it returns l2, and before that it splits the
 * head of l1 and concatenates it with the result
 * of append tail l2
*)

let rec append l1 l2 = 
  match l1 with
  | [] -> l2
  | head :: tail -> head :: (append tail l2)
;;

(* sumList : int list -> int 
 * (sumList n) is the sum of the numbers in the list
 * in n
 * e.g. (sumList [3,1,2,4,3] is 13
 *      (sumList [10,2,100] is 112 
   Decided to use recursion to work my way through the integers in the list inputted. First 
   I set a base case so that if the list in question is empty it will return an empty 
   list, as the value of the integer in question would be zero - as there is no
   integer there. However otherwise I remove the first element from the list, thus I get 
   a integer head and a integer list tail. I recurse on the tail value, and add the sum to
   the returned value. Thus when the tail is empty, it will return zero to the sumList called
   above it, which will then return its value, etc until all the recursions are done, and the
   summation is complete.
*) 

let rec sumList l =
  match l with
  | [] -> 0
  | (head::tail) -> sumList tail + head
  ;;




(* digitsOfInt : int -> int list 
 * (digitsOfInt n) is the list of digits of n (a positive integer)
 * in the order in which they appear in n
 * e.g. (digitsOfInt 31243) is [3,1,2,4,3]
 *      (digitsOfInt 23422) is [2,3,4,2,2]
 *)

let rec digitsOfInt n = 
  if n <= 0 
    then []
  else
     append (digitsOfInt (n/10)) [n mod 10]
;;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)
;;
(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence : int -> int
 * (additivePersistence n) is the number of additions required to obtain
 * a single digit from a number n
 * For example,  (additivePersistence 9876 ) is 2
 *)
let rec additivePersistence n =
 if n < 10 then 0
 else
   let x = digits n in
   let y = sumList x in
   if y<10 then 1
   else 1 + additivePersistence y 
;;


(* digitalRoot : int -> int
 * (digitalRoot n) is the digit obtained from a number n
 * when its digits are summed together again and again
 * For example, (digitalRoot 9876 ) is 3
 *)
let rec digitalRoot n = 
  if n < 10 then n
  else
    let x = digits n in
    let y = sumList x in
    if y < 10 then y
    else digitalRoot y
;;

(*         Part 2        *)

(* listReverse: 'a list -> 'a list 
 * check to see if the list is empty. If yes, return the list as is.
 * Otherwise look at list and try to split it by taking the head, and running the
 * function recursively on it, and appending the head to the return value 
 *)
let rec listReverse l =
  match l with
  | [] -> []
  | (head:: tail) -> append (listReverse tail ) [head] 
;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
  * I explode the input string to get a list. I then see if the string and the 
  * reverse of the string, obtained with listReverse, are identical. If
  * yes return True, else False
  *)
let palindrome w = 
    let x = explode w in
    let y = listReverse x in 
    if x = y then true
    else false 
;; 

(************** Add Testing Code Here ***************)

