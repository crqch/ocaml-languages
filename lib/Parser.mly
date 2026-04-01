%token <int> INT
%token MULT DIV ADD SUB
%token LPAR RPAR
%token EOF
%token NEG

%start <Ast.expr> main

%left ADD SUB
%left MULT DIV

%%

main:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | l = expr; MULT; r = expr { Binop (Mult, l, r) }
  | l = expr; DIV; r = expr { Binop (Div, l, r) }
  | l = expr; ADD; r = expr { Binop (Add, l, r) }
  | l = expr; SUB; r = expr { Binop (Sub, l, r) }
  | NEG; LPAR; e = expr; RPAR { Neg(e) }
  | LPAR; e = expr; RPAR { e }
  ;
