%{
  open Ast
%}

%token <int> INT
%token MULT DIV ADD SUB
%token LPAR RPAR
%token TRUE FALSE IF THEN ELSE
%token <string> IDENT
%token LET EQ IN
%token EOF

%start <Ast.expr> main

%left ADD SUB
%left MULT DIV

%%

main:
  | e = mixfix; EOF { e }
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If (e1,e2,e3) }
  | LET; i = IDENT; EQ; e1 = mixfix; IN; e2 = mixfix { Let (i, e1, e2) }
  | e = expr { e }
  ;

expr:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = IDENT { Var i }
  | l = expr; MULT; r = expr { Binop (Mult, l, r) }
  | l = expr; DIV; r = expr { Binop (Div, l, r) }
  | l = expr; ADD; r = expr { Binop (Add, l, r) }
  | l = expr; SUB; r = expr { Binop (Sub, l, r) }
  | LPAR; RPAR { Unit }
  | LPAR; e = mixfix; RPAR { e }
  ;
