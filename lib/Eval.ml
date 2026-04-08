open Ast

type value =
  | VInt of int
  | VBool of bool

let show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b

let print_value (v : value) : unit =
  v |> show_value |> print_string

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r  -> VInt (l / r)
  | Add, VInt l, VInt r  -> VInt (l + r)
  | Sub, VInt l, VInt r  -> VInt (l - r)
  | _ -> failwith "type error"

let reify (v : value) : expr =
  match v with
  | VInt a -> Int a
  | VBool a -> Bool a

let rec subst (x : ident) (v : value) (e : expr) : expr =
  match e with
  | Int _ -> e
  | Binop (op, l, r) ->
      Binop (op, subst x v l, subst x v r)
  | Bool _ -> e
  | If (b, t, e) ->
      If (subst x v b, subst x v t, subst x v e)
  | Let (y, e1, e2) ->
      if x = y
        then Let (y, subst x v e1, e2)
        else Let (y, subst x v e1, subst x v e2)
  | Var y ->
      if x = y
        then reify v
        else e

let rec eval (e : expr) : value =
  match e with
  | Int a -> VInt a
  | Binop (op, l, r) -> eval_op op (eval l) (eval r)
  | Bool b -> VBool b
  | If (b, t, e) ->
      (match eval b with
        | VBool true -> eval t
        | VBool false -> eval e
        | _ -> failwith "type error")
  | Let (x, e1, e2) ->
      eval (subst x (eval e1) e2)
  | Var y -> failwith ("unknown var " ^ y)

let interp (s : string) : value =
  let ast =
    Parser.main Lexer.read (Lexing.from_string s)
  in
  eval ast
