type token =
  | Num of (int)
  | TRUE
  | FALSE
  | Id of (string)
  | EOF
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LPAREN
  | RPAREN

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
