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

module M = Map.Make(String)

type env = value M.t

let empty_env  = M.empty

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int a -> VInt a
  | Binop (op, l, r) -> eval_op op (eval env l) (eval env r)
  | Bool b -> VBool b
  | If (b, t, e) ->
      (match eval env b with
        | VBool true -> eval env t
        | VBool false -> eval env e
        | _ -> failwith "type error")
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = M.add x v1 env in
      let v2 = eval env' e2 in
      v2
  | Var y ->
      (match M.find_opt y env with
       | Some v -> v
       | None -> failwith ("unknown var " ^ y))

let interp (s : string) : value =
  let ast =
    Parser.main Lexer.read (Lexing.from_string s)
  in
  eval empty_env ast
