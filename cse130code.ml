(*ocaml code*)


Lecture 2: 
match l with
| h::t ->
| [] -> true

;;


 let is_empty = fun l -> 
 match l with
  | [] ->true 
  | _::_ -> false 
  ;;

 let booleans_same = fun (b1, b2) -> 
 	match (b1, b2) with
 	| (true, true) -> true
 	| (false, false) -> true
 	| _ -> false
 	;;

 let plus = fun x ->  fun y -> x + y ;;

 let lt = va

Discussion 1:






type nat  =
| Z
| S of nat
;;


let rec to_int n =
	match n with
	| Z -> 0
	| S i -> 1 + to_int i ;;
let rec to_nat i  = 
	match i with
	| _ when i > 0-> S (to_nat (i-1))
	| _ -> Z
;;

let rec plus n m = 
	match n with 
	| Z -> m
	| S n' -> S (plus n' m)
;;

let rec mul;;

type int_list =
| Nil
| Cons of (int * int_list)
;;

let rec length l =
  match l with
  | Nil -> 0
  | Cons (h,t) -> 1 +length t
;;

let rec length2 l =
match l with
| [] -> 0
| h::t -> 1 + length2 t
;;

let sum l =
	let rec helper total remaining_list =
		match remaining_list with
		| [] -> total 
		| h::t -> helper(h+total) t
	in helper 0 l;;


let max x y = if x > y then x else y;;
let rec list_max xs =
	match xs with
	|[] ->  -19999999999
	| h::t -> max h (list_max t) 
;;


let rec filter f 1  =
	match l with 
	| [] -> []
	| (h::t) -> 
	
;;

lecture 6:
let rec max_list l =
	let rec helper max_so_far remaining_list
		match remaining_list with
		| [] -> max_so_far
		| h::t -> helper (max max_so_far h) t
	in helper 0 1;;


let rec fold f curr l =
	match l with 
		| [] -> curr
		| h::t -> fold f (f result)

lecture 8:

let map f l =
	let fold_fn acc elmt = acc @ [f elmt]  in
	let acc = [] in
  List.fold_left fold_fn acc l
;;

how it works:
acc = []
l =  [5:7:8]
f = (-) 1
fold_fn [] 5 
f 5 -> -4
fold_fn [-4] 7 -> [-4]; [-6]
fold_fn [-4;-6] -> [-4;-6;-7]

(**) partition

let partition f 1 =
	let base =
	let fold_fn acc elmnt =
	List.fold_left fold_fn base l
;;



(fold_left)  starts at the left of the list
	fold_left fold_fn acc 1

Lecture 9: 

let remove me l =
	let base = [] in
	let fold_fn = 

let fold_wih_index operator base l
	let base =  (base, 0) in
	let fold_fn (acc , idx) elmt = 
		let newAcc = operator (acc, idx) elmt 
		let acc' = ( newAcc, idx + 1) in
		in 
		fold_left fold_fn base l

	WTF
;;

let ith idx l default =
 let base = default in
 let fold_fn acc (elmt, index) = 
 	if (index = ith) then elmt else acc......... 

;; 


Lecture 10 Note:
  need to understand how ocaml keeps track of variable names
  let p = 0;;
  let q = "apple";;

  let z = (p,q);;
  let f x = p + x;;
  p =10 ;;
  f 0;;

  let f i = fun j -> i +j
  let g = f 10 ;;
  g 20;;
next example

let x  = 1;; 
let f y =
	let x = 2 in
	fun z -> x + y + z 
;;
let x  = 100


Lecture 11:

type tree =
| Leaf of int
| Node of tree * tree
| Empty;;

let x = Node(Node(Leaf 1, Leaf 2), Leaf 3)
let rec sum t =
	match t with
	| Node (t1, t2) -> sum t1 + sum t2
	| Leaf i -> i
	| Empty -> 0;;

let rec delete t i =
	match t with
	| Leaf j -> if (i = j) then Leaf 0 else Leaf j
	| Node (t1,t2) -> Node (delete t1 i , delete t2 i)
	| Empty -> Empty;;

// in java
Interface Tree 
{
   int sum();;
   Tree delete (int i);	
}
 class Leaf implements Tree
 {
  int data;
  Leaf(d) {this.data = 0;}
  int sum()
  {
  return this.data;
  }
  Tree delete(int i) { if (i == this.data) {; return new Leaf(0);} else return }
 }
 class Node implements Tree
 {
 	Tree left;
 	Tree right;
 	int sum()
 	{
 	return this.left.sum() +this.right.sum()
 	}
 	Tree delete (int i) {return Node(this.delete(i) , this.delete(j))}
 }

Tree x  = Leaf (10);
Tree x = new Node (new Leaf(10), new Leaf (20));
x.sum 

class Empty implements Tree 
{
	int sum() {return 0;}
	

}

Lecture 12:

//final words on functional programming
No while loops - just recursion
No mutability - once you create something it remains there forever
map , fold (map reduce) google
PL paraddigm - Ocaml- functional, Python -object oriented
Basic unit- expr value 
Source of imprecisios used to be printing

Overloading vs overriding
// string practice


