(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* assoc : int * string * (string * int) list
 * takes a triple (d,k,l) where l is a list of 
 * key-value pairs [(k1,v1);(k2,v2);...] and finds the first ki that equals k. 
 * If such a ki is found, then vi is returned. 
 * Otherwise, the default value d is returned.
 *)

let assoc (d,k,l) = 
  let rec assoc_helper (d,k,l) = 
  match l with
  | [] -> d 
  | (a,b)::tail -> if a = k then b else assoc_helper (d,k, tail)
  in assoc_helper (d, k, l) 
;;


(* removeDuplicates: int list -> int list
 * takes a list l and returns the list of elements of l
 * with the duplicates removed, and where the remaining elements
 * appear in the same order as in l .
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in 
    helper (seen',rest') 
  in
      List.rev (helper ([],l))
  ;;


(* 
 * wwhile : (int -> int * bool) * int -> int
 * takes as input a pair (f,b) and calls the function f on input b to get a pair (b',c').
 * wwhile should continue calling f on b' to update the pair as long as c' is true. 
 * Once f returns a c' that is false, wwhile should return b'.
 * 
 *)
let rec wwhile (f,b) = 
  let (b',c') = f b in
  if c' = false then b' else wwhile (f, b')
;;


(* fixpoint (int -> int) * int -> int
 * repeatedly updates b with f(b) until b=f(b) and then returns b.
 * Function declared inline as per talk with tutor 
 *)
let fixpoint (f,b) = wwhile ((fun  x -> if (f x) = x then (x, false) else ((f x), true)) ,b)
;;



(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)