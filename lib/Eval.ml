open Ast

let eval_op (op : bop) (l : int) (r : int) : int =
  match op with
  | Mult -> l * r
  | Div -> l / r
  | Add -> l + r
  | Sub -> l - r

let rec eval (e : expr) : int =
  match e with
  | Int a -> a
  | Binop (op, l, r) -> eval_op op (eval l) (eval r)

let interp (s : string) : int =
  let ast =
    Parser.main Lexer.read (Lexing.from_string s)
  in
  eval ast
