%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token <string> STRING
%token MULT DIV ADD SUB LT LEQ GT GEQ NEQ EQEQ EQ
%token LPAR RPAR LBRACE RBRACE SC
%token TRUE FALSE IF ELSE WHILE PRINT SKIP HALT
%token EOF

%start <Ast.stmt> main

%nonassoc LT LEQ GT GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%%

main:
  | e = stmts; EOF { e }
  ;

expr:
  | l = expr; MULT; r = expr { Binop (Mult, l, r) }
  | l = expr; DIV; r = expr { Binop (Div, l, r) }
  | l = expr; ADD; r = expr { Binop (Add, l, r) }
  | l = expr; SUB; r = expr { Binop (Sub, l, r) }
  | l = expr; LT; r = expr { Binop (Lt, l, r) }
  | l = expr; LEQ; r = expr { Binop (Leq, l, r) }
  | l = expr; GT; r = expr { Binop (Gt, l, r) }
  | l = expr; GEQ; r = expr { Binop (Geq, l, r) }
  | l = expr; NEQ; r = expr { Binop (Neq, l, r) }
  | l = expr; EQEQ; r = expr { Binop (Eq, l, r) }
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | s = STRING { Str s }
  | i = IDENT { Var i }
  | LPAR; e = expr; RPAR { e }
  ;

stmt:
  | s = matched_stmt { s }
  | s = unmatched_stmt { s }
  ;

matched_stmt:
  | x = IDENT; EQ; e = expr; SC { Assign (x, e) }
  | x = IDENT; MULT; EQ; e = expr; SC { Assign(x,Binop( Mult, Var x, e )) }
  | x = IDENT; DIV; EQ; e = expr; SC { Assign(x,Binop( Div, Var x, e )) }
  | x = IDENT; ADD; EQ; e = expr; SC { Assign(x,Binop( Add, Var x, e )) }
  | x = IDENT; SUB; EQ; e = expr; SC { Assign(x,Binop( Sub, Var x, e )) }
  | IF; LPAR; p = expr; RPAR; t = matched_stmt; ELSE; e = matched_stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = matched_stmt { While (p, b) }
  | PRINT; LPAR; e = expr; RPAR; SC { Print e }
  | LBRACE; s = stmts; RBRACE { s }
  | SKIP; SC { Skip }
  | HALT; SC { Halt }
  ;

unmatched_stmt:
  | IF; LPAR; p = expr; RPAR; t = stmt; { If (p, t, Skip) }
  | IF; LPAR; p = expr; RPAR; t = matched_stmt; ELSE; e = unmatched_stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = unmatched_stmt { While (p, b) }
  ;;

stmts:
  | { Skip }
  | s = stmt; ss = stmts { Cmp(s, ss) }
