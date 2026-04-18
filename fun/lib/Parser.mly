%{
  open Ast
%}

%token <int> INT
%token MULT DIV ADD SUB
%token LT LEQ GT GEQ NEQ EQEQ
%token LPAR RPAR COMMA
%token TRUE FALSE IF THEN ELSE
%token <string> IDENT
%token LET EQ IN
%token MATCH WITH ARR
%token FUN FUNREC
%token DEF
%token EOF

%start <Ast.expr> main
%start <Ast.def list> program

%nonassoc LT LEQ GT GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%%

main:
  | e = mixfix; EOF { e }
  ;

program:
  | EOF { [] }
  | DEF; i = IDENT; EQ; e = mixfix; p = program { (i, e) :: p }
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If (e1,e2,e3) }
  | LET; i = IDENT; EQ; e1 = mixfix; IN; e2 = mixfix { Let (i, e1, e2) }
  | MATCH; e1 = mixfix; WITH; LPAR; i1 = IDENT; COMMA; i2 = IDENT; RPAR; ARR; e2 = mixfix { Match (e1, i1, i2, e2) }
  | FUN; x = IDENT; ARR; e = mixfix { Fun (x, e) }
  | FUNREC; f = IDENT; x = IDENT; ARR; e = mixfix { Funrec (f, x, e) }
  | e = expr { e }
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
  | e = app { e }
  ;

app:
  | e1 = app; e2 = base { App (e1, e2) }
  | e = base { e }
  ;

base:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = IDENT { Var i }
  | LPAR; RPAR { Unit }
  | LPAR; e1 = mixfix; COMMA; e2 = mixfix; RPAR { Pair (e1, e2) }
  | LPAR; e = mixfix; RPAR { e }
  ;
