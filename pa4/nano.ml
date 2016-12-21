exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)
(* lookup: string * env -> value
 * finds the most recent binding for a variable (i.e. the first from the left)
 * in the list representing the environment
 *)
let lookup (x,evn) = 
  let a = listAssoc (x, evn) in 
  match a with 
  | Some value -> value 
  | None -> raise  ( MLFailure ("variable not bound: " ^ x ))
;;

(* eval : env * expr -> value
 * when called with the pair (evn,e) evaluates an ML-nano expression e of the above type, 
 * in the environment evn , and raises an exception MLFailure ("variable not bound: x") 
 * if the expression contains an unbound variable
 *)
let rec eval (evn,e) = 
  (*match the expression*)
  match e with
  (*const, true, false*)
  | Const i -> Int i
  | True -> Bool true 
  | False -> Bool false
  (* if statement*)
  | If (e1,e2,e3) -> 
      let l1 = eval (evn , e1) in
      ( match l1 with 
      |  Bool b1 -> if b1 then (eval (evn, e2))  else (eval (evn,e3) )
      |  _ -> raise (MLFailure ("Invalid statement") )
      )
  (* match with var*)
  | Var s -> 
    lookup (s,evn)
  (*Let and let rec*)
  | Let (s, e1, e2) -> 
    let x1 = eval (evn, e1) in
    let eval_evn = (s,x1)::evn in
    eval(eval_evn, e2)
  | Letrec (s, e1, e2) ->
    let x1 = eval (evn, e1) in
    (match x1 with
     | Closure (evn1, stro, str, e) -> eval ( ( (s , ( Closure (evn1, Some s, str, e) ) ) :: evn) ,  e2)
     | _ -> eval ((s, x1)::evn, e2)
    )
  (*function application*)  
  | App (e1, e2) -> 
    let Closure (evn2, name, s, e) = eval (evn, e1) in
    let x1 = eval (evn, e2) in
    let temp = 
    (match name with
    | Some a -> ((a, Closure (evn2, name, s, e) ) :: ((s, x1) :: evn2 ))
    | None -> ((s, x1 ) :: evn2 )
    | _ -> raise (MLFailure ("Invalid statement") )
    ) in
    eval (temp, e)
  | Fun (s, e1) -> Closure (evn, None, s , e1  )
  (*binary operators*)
  | Bin (e1, op, e2) -> 
    let val1 = eval (evn, e1) in
    let val2 = eval (evn, e2 )in
    ( match (val1, op, val2) with
    | (Int i1,Plus, Int i2 ) -> Int ( i1 + i2 )
    | (Int i1,Minus, Int i2 )  -> Int ( i1 - i2)
    | (Int i1, Mul, Int i2 ) -> Int (i1 * i2)
    | (Int i1, Div, Int i2 ) ->  Int (i1 / i2)
    | (Bool b1, And, Bool b2) -> Bool (b1 && b2)
    | (Bool b1, Or, Bool b2) -> Bool (b1 || b2)
    | (Bool b1, Eq, Bool b2) -> Bool ( b1 = b2)  
    | (Int i1, Eq, Int i2) -> Bool ( i1 = i2)
    | (Bool b1, Ne, Bool b2) -> Bool (b1 != b2)
    | (Int i1, Ne, Int i2) -> Bool (i1 != i2)
    | (Int i1, Lt, Int i2) -> Bool (i1 < i2)
    | (Int i1, Le, Int i2) -> Bool (i1 <= i2)
    | _ -> raise (MLFailure ("Invalid statement") ) 
    )
  (* if it is anything else, raise an error*)
  | _ -> raise (MLFailure ("Invalid statement") )


(**********************     Testing Code  ******************************)
