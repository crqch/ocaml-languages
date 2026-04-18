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
%token FUN FUNREC REC
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
  | DEF; i = IDENT; xs = ids; EQ; e = mixfix; p = program { (i, List.fold_right (fun x acc -> Fun(x, acc)) xs e) :: p }
  | DEF; REC; i = IDENT; xs = ids; EQ; e = mixfix; p = program {
    (i, Funrec(i, List.hd xs, List.fold_right (fun x acc -> Fun(x, acc)) (List.tl xs) e)) :: p
  }
  ;

ids:
  | i = IDENT { [i] }
  | i = IDENT; rest = ids { i :: rest }
  ;

tuples:
  | i = mixfix { i }
  | i = mixfix; COMMA; rest = tuples { Pair(i, rest) }
  ;

tuple_idents:
  | i = IDENT { [i] }
  | i = IDENT; COMMA; rest = tuple_idents { i :: rest }
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If (e1,e2,e3) }
  | LET; i = IDENT; EQ; e1 = mixfix; IN; e2 = mixfix { Let (i, e1, e2) }
  | MATCH; e1 = mixfix; WITH; LPAR; ids = tuple_idents; RPAR; ARR; e2 = mixfix { Match (e1, ids, e2) }
  | FUN; xs = ids; ARR; e = mixfix {
    List.fold_right (fun x acc -> Fun (x, acc)) xs e
  }
  | FUNREC; f = IDENT; xs = ids; ARR; e = mixfix {
    Funrec(f, List.hd xs, List.fold_right (fun x acc -> Fun(x, acc)) (List.tl xs) e)
  }
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
  | LPAR; es = tuples; RPAR { es }
  | LPAR; e = mixfix; RPAR { e }
  ;
