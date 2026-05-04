%{
  open Ast
%}

%token <Bigint.t> INT
%token <string> IDENT
%token <string> STRING
%token MULT DIV ADD SUB LT LEQ GT GEQ NEQ EQEQ EQ
%token LPAR RPAR LBRACE RBRACE LSQU RSQU SC
%token TRUE FALSE IF ELSE WHILE FOR PRINT SKIP HALT
%token EOF

%start <Ast.stmt> main

%nonassoc LT LEQ GT GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%%

main:
  | e = stmts; EOF { e }
  ;


array_elems:
  | l = expr; SC; r = array_elems { l :: r }
  | l = expr { [l] }
  | { [] }

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
  | LBRACE; elems = array_elems; RBRACE { Array(elems) }
  | i = IDENT; LSQU; e = expr; RSQU { ArrayRead(i, e) }
  ;



stmt:
  | s = matched_stmt { s }
  | s = unmatched_stmt { s }
  ;


expr_opt:
  | e = expr; SC { e }
  | SC { Bool true }
  ;

init:
  | x = IDENT; EQ; e = expr; SC { Assign (x, e) }
  | SC { Skip }
  ;

step_stmt:
  | x = IDENT; EQ; e = expr { Assign (x, e) }
  | x = IDENT; LSQU; idx = expr; RSQU; EQ; v = expr { ArrayWrite(x, idx, v) }
  | x = IDENT; MULT; EQ; e = expr { Assign(x,Binop( Mult, Var x, e )) }
  | x = IDENT; DIV; EQ; e = expr { Assign(x,Binop( Div, Var x, e )) }
  | x = IDENT; ADD; EQ; e = expr { Assign(x,Binop( Add, Var x, e )) }
  | x = IDENT; SUB; EQ; e = expr { Assign(x,Binop( Sub, Var x, e )) }
  | IF; LPAR; p = expr; RPAR; t = matched_stmt; ELSE; e = matched_stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = matched_stmt { While (p, b) }
  | FOR; LPAR; i = init; cond = expr_opt; s = step_stmt; RPAR; body = matched_stmt { Cmp(i, While(cond, Cmp(body, s))) }
  | PRINT; LPAR; e = expr; RPAR { Print e }
  | LBRACE; s = stmts; RBRACE { s }
  | SKIP { Skip }
  | HALT { Halt }
  | { Skip }
  ;


matched_stmt:
  | x = IDENT; EQ; e = expr; SC { Assign (x, e) }
  | x = IDENT; MULT; EQ; e = expr; SC { Assign(x,Binop( Mult, Var x, e )) }
  | x = IDENT; DIV; EQ; e = expr; SC { Assign(x,Binop( Div, Var x, e )) }
  | x = IDENT; ADD; EQ; e = expr; SC { Assign(x,Binop( Add, Var x, e )) }
  | x = IDENT; SUB; EQ; e = expr; SC { Assign(x,Binop( Sub, Var x, e )) }
  | x = IDENT; LSQU; idx = expr; RSQU; EQ; v = expr; SC { ArrayWrite(x, idx, v) }
  | IF; LPAR; p = expr; RPAR; t = matched_stmt; ELSE; e = matched_stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = matched_stmt { While (p, b) }
  | FOR; LPAR; i = init; cond = expr_opt; s = step_stmt; RPAR; body = matched_stmt { Cmp(i, While(cond, Cmp(body, s))) }
  | PRINT; LPAR; e = expr; RPAR; SC { Print e }
  | LBRACE; s = stmts; RBRACE { s }
  | SKIP; SC { Skip }
  | HALT; SC { Halt }
  ;

unmatched_stmt:
  | IF; LPAR; p = expr; RPAR; t = stmt; { If (p, t, Skip) }
  | IF; LPAR; p = expr; RPAR; t = matched_stmt; ELSE; e = unmatched_stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = unmatched_stmt { While (p, b) }
  | FOR; LPAR; i = init; cond = expr_opt; s = step_stmt; RPAR; body = unmatched_stmt { Cmp(i, While(cond, Cmp(body, s))) }
  ;;

stmts:
  | { Skip }
  | s = stmt; ss = stmts { Cmp(s, ss) }
