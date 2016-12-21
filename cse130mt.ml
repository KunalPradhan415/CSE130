(*cse130mt prep final winter 2012*)
 let key_max_val d =
 (*def of fold_fn*)
  let f (strKey, value) (strKey2, value2) = if value2 > value then (strKey2, value2) else (strKey, value)
  in 
  match d with
  | [] -> raise Not_found
  | base::t -> let (a,b) = (fold_left f base t ) in a
;;

(*midterm winter 2012*)
let rec split l =
	let lenby2 = (List.length l ) /2 in
	let fold_fn  (i,l1, l2) elmt  = if (i < lenby2 ) then (i+1, l1 @ [elmt], l2 ) else (i+1, l1, l2 @ [elmt])in 	
	let base = (0, [], []) in
    let (_, l1, l2) = List.fold_left fold_fn base l in
	 (l1,l2)
	;;

let rec merge l1 l2 =
	match (l1,l2) with
	| ([], l) -> l
	| (l, []) -> l
	| (h1::t1,h2::t2) -> if h1 < h2 then h1::(merge t1 l2) else h2::(merge l1 t2)
;;

let rec merge_sort l = 
	match l with
	| [] -> []
	| [a] -> [a]
	| [a; b] -> merge [a] [b] 
	| _ -> let (a,b) = split l in
	 		merge ( merge_sort a) (merge_sort b) 
	 ;;

let replace s = 
	let l = explode s in
	let mapfn c = if (c = '-') then ' ' else c in
	let d = List.map mapfn l in
	let ret = implode d in
	ret;;

let app l x =
	let mapfun f = f x in
	List.map mapfun l
;;

(*midterm winter 2013*)
type 'a maybe =
| None
| Some of 'a;;

let first f l =
	let base = (None) in
	let fold_fn acc elmt =
		if (f elmt = true) then if (acc = (None) ) then elmt else acc
		else acc
	in 
	List.fold_left fold_fn base l
;;

let first f l =
  let base = (None) in
  let fold_fn acc elmt = 
    match acc with 
  	| None -> if (f elmt) then Some elmt else None
   	| Some x -> Some x
   in 
   List.fold_left fold_fn base l 

let rec zip l1 l2 =
	match (l1,l2) with
	| ((h1::t1), (h2::t2)) -> (h1, h2):: (zip t1 t2)
	| _ -> []
;;

zip ['a';'b';'c'] [1;2;3];;
zip ['a'] [1;2;3];;
zip ['a';'b';'c'] [1;2];;

let map2 f l1 l2 = 
	let x = zip l1 l2 in
	let g (x1,x2) = f x1 x2 in
	List.map g x
;;

let map3 f l1 l2 l3 =
	let x = zip l1 l2 in
	let y = zip x l3 in
	let g ((x1, x2), x3) = f x1 x2 x3 in
	List.map g y
;;

(* fall 2013 midterm*)
let count l x =
	let fold_fn a elmt = if (x = elmt) then (a+1) else a in
	let base = 0 in
	List.fold_left fold_fn base l
;;


let make_palyndrome l =
	let fold_fn a x = x::a in
	let base = l in
	List.fold_left fold_fn base l
;;

let weird l =
	let fn acc e i = (acc + e + i) in
	let base = 0 in
	fold_2 fn base l
;;

let fold_2 f b l =
	let base = (b, 0) in
	let fold_fn (a1, index) s = (f a1 s (index ) , index +1) in
	let (x, y ) = List.fold_left fold_fn base l in
	x
;;

let rec ith l i d =
	let fold_fn a e indx = if (indx = i) then e else a in
	let base  = d in
	fold_2 fold_fn base l 
;;




type 'a fun_tree =
| Leaf of ( 'a -> 'a)
| Node of ('a fun_tree) * ('a fun_tree)
;;

let rec apply_all t x =
	match t with
	| Leaf f -> f x
	| Node (l,r) -> apply_all r (apply_all l x)
;;

//
let rec compose t1 t2 =
	match (t1,t2) with
	| (Leaf l, Leaf r) ->  Leaf ( fun x ->  r (l x) )  
	| (Node (n1,n2 ), Node (n3, n4) ) ->   Node (compose n1 n3,compose n2 n4 )
;;

(* midterm spring 2013*)
let length l =
	let fold_fn a x  = (a+1) in
	let base = 0 in
	List.fold_left fold_fn base l 
;;

let remove l x =
	let fold_fn a elmt = if ( elmt = x ) then a else (a @ [elmt]) in
	let base = [] in
	List.fold_left fold_fn base l
;;

let rec ith l i d =
	match l with
	| [] ->  d 
	| h::t -> if (i = 0) then h else (ith t (i-1 ) d )
;;

let rec update l i n =
	match l with
	| [] -> []
	| h::t -> if (i = 0) then ([n] @ t ) else h:: (update t (i-1) n ) 
;;

let rec update2 l i n d =
	match l with
	| [] ->   ( update2 (l@ [d]) i n d )
	| h::t -> if (i = 0) then ([n] @ t ) else h:: (update2 t (i-1) n d) 
;;

let categorize f l =
	let base = [] in
	let fold_fn acc elmt = 
	let idx = ( f elmt) in
	let bin = ith acc idx []  in
	if bin = []  then update2 acc idx [elmt] [] else update2 acc idx ( bin @ [elmt] ) []
	in List.fold_left fold_fn base l 
;;

//redoing practice
let first f l = 
	let base = (None) in
	let fold_fn acc elmt = 
		let value = f elmt in
		if value = true then if acc != None  then acc else Some elmt
		else acc 
in List.fold_left fold_fn base l
;;


let rec zip l1 l2 =
	match (l1,l2) with
	| (l, []) -> []
	| ([], l) -> []
	| ((h1::t1), (h2::t2)) -> (h1,h2) :: (zip t1 t2 )
;;

let map2 f l1 l2 =
	let x = zip l1 l2 in
	let g (x1,x2) = f x1 x2 in
	List.map g x 
;;

let map3 f l1 l2 l3 =
	let a = zip l1 l2 in
	let b = zip a l3 in
	let g (( x1, x2 ), x3) = f x1 x2 x3 in
	List.map g b
;;

let rec unzip l =
	match l with 
	| [] -> ([],[])
	| (h1,h2) :: t -> let (n1, n2) = (unzip t ) in 
	(h1::n1, h2::n2 ) 
	;;    





