{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}
(*lexer*)
rule token = parse
    eof         		{ EOF }
  | "true"					{TRUE}
  | "false"					{FALSE}
  | [' ' '\t' '\n' '\r']    { token lexbuf } 
  | "let"					{LET}
  | "rec"					{REC}
  | "="						{EQ}
  | "in"					{IN}
  | "fun"					{FUN}
  | "->"					{ARROW}
  | "if"					{IF}
  | "then"				{THEN}
  | "else"				{ELSE}
  | "("						{LPAREN}
  | ")"						{RPAREN}
  | "+"						{PLUS}
  | "-"						{MINUS}
  | "*"						{MUL}
  | "/"						{DIV}
  | "<"						{LT}
  | "<="					{LE}
  | "!="					{NE}
  | "&&"					{AND}
  | "||"					{OR}
  | ['0'-'9']+ as n			{ Num(int_of_string n) }
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as str    	{ Id(str) }
  | _   { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
 