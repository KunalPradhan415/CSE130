(*
midterm2014cse130
*)


(*Redo*)
type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;


let rec rename_var e n1 n2 =
	match e with
	| Const i -> Const i
	| Var s -> if s = n1 then Var n2 else Var s
	| Op (str, e1, e2) -> Op (str, rename_var e1 n1 n2, rename_var e2 n1 n2)
;;

let to_str e =
	let rec str_helper e top_level =
		match e with 
		| Const i -> string_of_int i
		| Var s -> s
		| Op (str, e1, e2) -> if top_level = true then (str_helper e1 false) ^ str ^ (str_helper e2 false) else "(" ^ (str_helper e1 false) ^ str ^ (str_helper e2 false) ^ ")"
		in 
		str_helper e true
;;


let average_if f l =
	let folding_fn (sum, num) elmt = if (f elmt) = true 
		then (sum+elmt, num + 1) else (sum, num) in
	let base = (0,0 )in
	let (x, y) = List.fold_left folding_fn base l in
	if y = 0 then 0 else x/y
;;

let length_2 l = List.fold_left  (+) 0 (List.map List.length l);;

let length_3 l = List.fold_left  (+) 0 (List.map length_2 l);;
































(*Problem1*)
type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;

let rec rename_var e n1 n2 =
	match e with
	| Const i -> Const i 
	| Var s -> if s = n1 then Var n2 else Var s
	| Op (s1, e1,e2) -> Op (s1, rename_var e1 n1 n2 , rename_var e2 n1 n2) 
;;

let to_str e =
	let rec str_helper e top_level = 
		match e with
		| Const i -> string_of_int i
		| Var s -> s
		| Op (s1, e1, e2) -> if top_level = true then (str_helper e1 false)^s1^(str_helper e2 false) 
						else "(" ^ ( str_helper e1 false)^ s1 ^ (str_helper e2 false) ^ ")"
	in
	str_helper e true;;

(*Problem 2*)
let average_if f l =
	let folding_fn (sum, count) x =
	if f x = true then (sum + x, count + 1)
	else (sum, count) in
let base = (0,0) in
let (a,b) = List.fold_left folding_fn base l in
match (a,b) with
| (0,0) -> 0
| (sum, count) -> sum/count
;;

let x = average_if (fun x -> if (x mod 2) = 0 then true else false) [1;2;3;4];;

(*problem 3*)
let length_2 l = List.fold_left (+) 0 (List.map List.length l) ;;
let length_3 l = List.fold_left (+) 0 (List.map length_2 l );;