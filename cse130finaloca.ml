(*winter 2012 final*)

let rec find d k =
match d with
| [] -> raise Not_found
| (k',v') :: t -> if k' > k then v' else if k' = k  then ( raise Not_found ) else ( find t k )
;;


let rec add d k v =
match d with
| [] -> [(k,v)]
| (k',v') :: t -> if k' == k then ( (k,v) :: t ) else if k' > k then ( (k,v) :: d ) else (k',v') :: ( add t k v )
;;

let keys d =
	let map_fn (a1, a2) = a1 in
	List.map map_fn d
;;

let values d =
	let map_fn (a1, a2) = a2 in
	List.map map_fn d
;;

let key_of_max_val d =
	let fold_fn  (a1,a2) (b1,b2) =  if b2 > a2 then (b1,b2) else (a1,a2) in
	match d with
	| [] -> raise Not_found
	| base::t ->  let (res,_) = List.fold_left fold_fn base t in
				   res
;;







(*winter 2013*)
let sum_matrix l =
	let base = 0 in
	let fold_fn  a x =
		let base2 = a in
		let fold_fn2 b h = b + h in
		List.fold_left fold_fn2 base2 x
	in
	List.fold_left fold_fn base l
	;;















(*cse130 fall 2013 final *)
let rec insert l i =
match l with
| [] -> [i]
| (h::t) -> if i <= h then  (i :: l) else h :: (insert t i)
;;

let insertion_sort = List.fold_left insert [] ;;

let insertion_sort l = List.fold_left insert [] l ;;

type expr =
| Var of string
| Const of int
| Plus of expr * expr
;;

let rec simpl e =
match e with
| Plus (e1, e2) ->
	( 
	let e1' = simpl e1 in
	let e2' = simpl e2 in
	match (e1',e2') with
	| (Const i1, Const i2) -> Const (i1 + i2)
	| _ ->  Plus (e1', e2')
	)
| _ -> e
;;























(*cse130 final spring 13 *)
let count f l  = 
	let fold_fn a x = if f x = true then a+1 else a in
	let base = 0 in
	let temp = (List.fold_left fold_fn base l) in
	temp 
;;

let stretch l =
	let fold_fn a x = ((a@[x])@[x]) in
	let base = [] in
	List.fold_left fold_fn base l
;;

type 'a tree =
| Empty
| Node of 'a * 'a tree list
;;

let rec zip l1 l2 =
match (l1,l2) with
| ([],[]) -> []
| (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
| _ -> raise (Failure "foo");;

let rec tree_zip t1 t2 =
match (t1,t2) with
| (Empty, Empty) -> Empty
| (Node(h1,t1), Node (h2,t2)) -> 
	let myList = zip t1 t2 in
	let map_fn (f1,f2) = tree_zip f1 f2 in
	let res = List.map map_fn myList in
	Node([h1,h2], res )
| _ -> raise (Failure "foo");;

(*winter 2013*)
let rec insert l i =
	match l with
	| [] -> [i]
	| (h::t) -> if h > i then ( [i] @ l) else (h ::(insert t i) )
;;

let insertion_sort =


(*cse130 final fall 05 *)
(*05 final*)

let rec ru (f,g,base) =
if (g base) then ru (f,g,(f base))
else base
;;

let reverse l = 
	let f (h::t, l2) = (t, h::l2)  in
	let g (x1,x2) = if x1 = [] then false else true in
	let base = (l, []) in
	let (_,r) = ru(f,g,base) in
	r
;;


(*e1*)
let x = a + 1 in 
let y = b + 2 in 
2*x + 3*y 

(*e1 e2*)
let y = b + 2 in
let x = a + 1 in
3*y + 2*x
