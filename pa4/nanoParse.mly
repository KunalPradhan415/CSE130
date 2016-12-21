%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token TRUE
%token FALSE
%token <string> Id 
%token EOF

%token LET
%token REC
%token EQ
%token IN
%token FUN
%token ARROW
%token IF 
%token THEN
%token ELSE 

%token PLUS
%token MINUS
%token MUL
%token DIV
%token LT
%token LE
%token NE
%token AND
%token OR

%token LPAREN
%token RPAREN

%start exp 
%type <Nano.expr> exp

%left MUL DIV PLUS MINUS EQ NE LT LE AND OR

%%

exp:

| LET REC Id EQ exp IN exp 		{ Letrec ($3, $5, $7) }
| LET Id EQ exp IN exp			{ Let ($2,$4, $6) }
| IF exp THEN exp ELSE exp 		{ If ($2,$4, $6) }
| FUN Id ARROW exp 				{ Fun ($2, $4) }
| exp_or						{$1}

exp_or:
| exp_or OR exp_and 			{ Bin ($1, Or, $3) }	
| exp_and						{$1}

exp_and:
| exp_and AND exp_arith 		{ Bin ($1, And, $3) }
| exp_arith						{$1}

exp_arith:
| exp_arith EQ exp_pm 			{ Bin ($1, Eq, $3) }
| exp_arith NE exp_pm 			{ Bin ($1, Ne, $3) }
| exp_arith LT exp_pm 			{ Bin ($1, Lt, $3) }
| exp_arith LE exp_pm 			{ Bin ($1, Le, $3) }
| exp_pm						{$1}

exp_pm:
| exp_pm PLUS exp_md 			{ Bin ($1, Plus, $3) }
| exp_pm MINUS exp_md 			{ Bin ($1, Minus, $3) }
| exp_md						{$1}

exp_md:
| exp_md MUL exp_fun 			{ Bin ($1, Mul, $3) }
| exp_md DIV exp_fun 			{ Bin ($1, Div, $3) }
| exp_fun						{$1}

exp_fun:
| exp_fun exp_base				{ App ($1, $2) }
| exp_base							{$1}

exp_base: 
| Num                        	{ Const $1 }
| TRUE							{ True}
| FALSE							{ False}
| Id							{ Var $1}
| LPAREN exp RPAREN				{ $2 }
			



