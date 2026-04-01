type bop = Mult | Div | Add | Sub

type expr =
  | Int of int
  | Binop of bop * expr * expr
  | Neg of expr
