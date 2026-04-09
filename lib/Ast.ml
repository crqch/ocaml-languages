type bop = Mult | Div | Add | Sub

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Let of ident * expr * expr
  | Match of expr * ident * ident * expr
  | Sum of ident * expr * expr * expr
  | Var of ident
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Unit
