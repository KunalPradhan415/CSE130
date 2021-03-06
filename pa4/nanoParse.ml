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

open Parsing;;
let _ = parse_error;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
# 35 "nanoParse.ml"
let yytransl_const = [|
  258 (* TRUE *);
  259 (* FALSE *);
    0 (* EOF *);
  261 (* LET *);
  262 (* REC *);
  263 (* EQ *);
  264 (* IN *);
  265 (* FUN *);
  266 (* ARROW *);
  267 (* IF *);
  268 (* THEN *);
  269 (* ELSE *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* MUL *);
  273 (* DIV *);
  274 (* LT *);
  275 (* LE *);
  276 (* NE *);
  277 (* AND *);
  278 (* OR *);
  279 (* LPAREN *);
  280 (* RPAREN *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  260 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\005\000\005\000\
\005\000\006\000\006\000\006\000\007\000\007\000\008\000\008\000\
\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\007\000\006\000\006\000\004\000\001\000\003\000\001\000\003\000\
\001\000\003\000\003\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\023\000\024\000\025\000\026\000\000\000\000\000\
\000\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\002\000\000\000\003\000\001\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000"

let yysindex = "\255\255\
\004\255\000\000\000\000\000\000\000\000\000\000\006\255\007\255\
\004\255\004\255\000\000\248\254\253\254\034\255\002\255\008\255\
\019\255\000\000\022\255\030\255\016\255\028\255\021\255\019\255\
\019\255\019\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\000\000\004\255\036\255\004\255\004\255\000\000\253\254\
\034\255\002\255\002\255\002\255\002\255\008\255\008\255\019\255\
\019\255\038\255\004\255\000\000\035\255\004\255\042\255\004\255\
\000\000\004\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\112\000\217\000\196\000\109\000\055\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\220\000\
\202\000\127\000\145\000\163\000\181\000\073\000\091\000\019\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\000\000\036\000\038\000\030\000\002\000\006\000\243\255"

let yytablesize = 500
let yytable = "\001\000\
\020\000\022\000\023\000\034\000\003\000\004\000\005\000\006\000\
\007\000\019\000\021\000\020\000\008\000\024\000\009\000\030\000\
\031\000\025\000\018\000\003\000\004\000\005\000\006\000\032\000\
\033\000\037\000\010\000\050\000\035\000\052\000\053\000\046\000\
\047\000\036\000\034\000\034\000\019\000\048\000\049\000\038\000\
\026\000\010\000\051\000\055\000\039\000\054\000\057\000\056\000\
\059\000\058\000\060\000\027\000\028\000\029\000\017\000\042\000\
\043\000\044\000\045\000\040\000\000\000\000\000\041\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\000\000\
\020\000\018\000\018\000\000\000\000\000\000\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\000\000\018\000\019\000\019\000\000\000\000\000\000\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\000\000\019\000\017\000\017\000\000\000\
\000\000\000\000\017\000\017\000\017\000\017\000\000\000\000\000\
\017\000\017\000\017\000\017\000\017\000\000\000\017\000\015\000\
\015\000\000\000\000\000\000\000\015\000\015\000\015\000\015\000\
\000\000\000\000\015\000\015\000\015\000\015\000\015\000\000\000\
\015\000\016\000\016\000\000\000\000\000\000\000\016\000\016\000\
\016\000\016\000\000\000\000\000\016\000\016\000\016\000\016\000\
\016\000\000\000\016\000\014\000\014\000\000\000\000\000\005\000\
\014\000\014\000\000\000\005\000\005\000\000\000\014\000\014\000\
\014\000\014\000\014\000\000\000\014\000\010\000\010\000\005\000\
\000\000\000\000\010\000\010\000\000\000\000\000\000\000\000\000\
\010\000\010\000\010\000\010\000\010\000\000\000\010\000\012\000\
\012\000\000\000\000\000\000\000\012\000\012\000\000\000\000\000\
\000\000\000\000\012\000\012\000\012\000\012\000\012\000\000\000\
\012\000\013\000\013\000\000\000\000\000\000\000\013\000\013\000\
\000\000\000\000\000\000\000\000\013\000\013\000\013\000\013\000\
\013\000\000\000\013\000\011\000\011\000\000\000\000\000\000\000\
\011\000\011\000\000\000\000\000\000\000\000\000\011\000\011\000\
\011\000\011\000\011\000\009\000\011\000\000\000\000\000\009\000\
\009\000\008\000\000\000\000\000\000\000\008\000\008\000\000\000\
\009\000\009\000\000\000\009\000\000\000\000\000\008\000\008\000\
\007\000\008\000\000\000\006\000\007\000\007\000\000\000\006\000\
\006\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\007\000\006\000\000\000\006\000"

let yycheck = "\001\000\
\000\000\009\000\010\000\017\000\001\001\002\001\003\001\004\001\
\005\001\004\001\004\001\006\001\009\001\022\001\011\001\014\001\
\015\001\021\001\000\000\001\001\002\001\003\001\004\001\016\001\
\017\001\010\001\023\001\035\000\007\001\037\000\038\000\030\000\
\031\000\004\001\048\000\049\000\000\000\032\000\033\000\012\001\
\007\001\023\001\007\001\051\000\024\001\008\001\054\000\013\001\
\056\000\008\001\058\000\018\001\019\001\020\001\000\000\026\000\
\027\000\028\000\029\000\024\000\255\255\255\255\025\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\007\001\008\001\255\255\255\255\008\001\
\012\001\013\001\255\255\012\001\013\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\007\001\008\001\024\001\
\255\255\255\255\012\001\013\001\255\255\255\255\255\255\255\255\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\255\255\255\255\
\255\255\255\255\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\255\255\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\019\001\
\020\001\021\001\022\001\008\001\024\001\255\255\255\255\012\001\
\013\001\008\001\255\255\255\255\255\255\012\001\013\001\255\255\
\021\001\022\001\255\255\024\001\255\255\255\255\021\001\022\001\
\008\001\024\001\255\255\008\001\012\001\013\001\255\255\012\001\
\013\001\255\255\255\255\255\255\255\255\255\255\022\001\255\255\
\024\001\022\001\255\255\024\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  EOF\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  FUN\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  LT\000\
  LE\000\
  NE\000\
  AND\000\
  OR\000\
  LPAREN\000\
  RPAREN\000\
  "

let yynames_block = "\
  Num\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 45 "nanoParse.mly"
                             ( Letrec (_3, _5, _7) )
# 286 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 46 "nanoParse.mly"
                         ( Let (_2,_4, _6) )
# 295 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 47 "nanoParse.mly"
                             ( If (_2,_4, _6) )
# 304 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 48 "nanoParse.mly"
                       ( Fun (_2, _4) )
# 312 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_or) in
    Obj.repr(
# 49 "nanoParse.mly"
              (_1)
# 319 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_or) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_and) in
    Obj.repr(
# 52 "nanoParse.mly"
                       ( Bin (_1, Or, _3) )
# 327 "nanoParse.ml"
               : 'exp_or))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_and) in
    Obj.repr(
# 53 "nanoParse.mly"
               (_1)
# 334 "nanoParse.ml"
               : 'exp_or))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_and) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_arith) in
    Obj.repr(
# 56 "nanoParse.mly"
                          ( Bin (_1, And, _3) )
# 342 "nanoParse.ml"
               : 'exp_and))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_arith) in
    Obj.repr(
# 57 "nanoParse.mly"
                 (_1)
# 349 "nanoParse.ml"
               : 'exp_and))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_pm) in
    Obj.repr(
# 60 "nanoParse.mly"
                         ( Bin (_1, Eq, _3) )
# 357 "nanoParse.ml"
               : 'exp_arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_pm) in
    Obj.repr(
# 61 "nanoParse.mly"
                         ( Bin (_1, Ne, _3) )
# 365 "nanoParse.ml"
               : 'exp_arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_pm) in
    Obj.repr(
# 62 "nanoParse.mly"
                         ( Bin (_1, Lt, _3) )
# 373 "nanoParse.ml"
               : 'exp_arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_pm) in
    Obj.repr(
# 63 "nanoParse.mly"
                         ( Bin (_1, Le, _3) )
# 381 "nanoParse.ml"
               : 'exp_arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_pm) in
    Obj.repr(
# 64 "nanoParse.mly"
              (_1)
# 388 "nanoParse.ml"
               : 'exp_arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_pm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_md) in
    Obj.repr(
# 67 "nanoParse.mly"
                        ( Bin (_1, Plus, _3) )
# 396 "nanoParse.ml"
               : 'exp_pm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_pm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_md) in
    Obj.repr(
# 68 "nanoParse.mly"
                         ( Bin (_1, Minus, _3) )
# 404 "nanoParse.ml"
               : 'exp_pm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_md) in
    Obj.repr(
# 69 "nanoParse.mly"
              (_1)
# 411 "nanoParse.ml"
               : 'exp_pm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_md) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_fun) in
    Obj.repr(
# 72 "nanoParse.mly"
                        ( Bin (_1, Mul, _3) )
# 419 "nanoParse.ml"
               : 'exp_md))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_md) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_fun) in
    Obj.repr(
# 73 "nanoParse.mly"
                        ( Bin (_1, Div, _3) )
# 427 "nanoParse.ml"
               : 'exp_md))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_fun) in
    Obj.repr(
# 74 "nanoParse.mly"
               (_1)
# 434 "nanoParse.ml"
               : 'exp_md))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp_fun) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp_base) in
    Obj.repr(
# 77 "nanoParse.mly"
                      ( App (_1, _2) )
# 442 "nanoParse.ml"
               : 'exp_fun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp_base) in
    Obj.repr(
# 78 "nanoParse.mly"
                 (_1)
# 449 "nanoParse.ml"
               : 'exp_fun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "nanoParse.mly"
                              ( Const _1 )
# 456 "nanoParse.ml"
               : 'exp_base))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "nanoParse.mly"
             ( True)
# 462 "nanoParse.ml"
               : 'exp_base))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "nanoParse.mly"
              ( False)
# 468 "nanoParse.ml"
               : 'exp_base))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "nanoParse.mly"
           ( Var _1)
# 475 "nanoParse.ml"
               : 'exp_base))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    Obj.repr(
# 85 "nanoParse.mly"
                       ( _2 )
# 482 "nanoParse.ml"
               : 'exp_base))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Nano.expr)
