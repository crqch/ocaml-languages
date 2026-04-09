type bop = Mult | Div | Add | Sub

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Let of ident * expr * expr
  | Var of ident
  | Unit
