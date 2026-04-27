open Ast

type value =
  | VInt of int
  | VBool of bool
  | VStr of string

module Memory : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type memory = value Memory.t

let rec show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b
  | VStr s -> s

let print_value (v : value) : unit =
  v |> show_value |> print_string

let rec equal_value (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VInt a1, VInt a2 -> Int.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VStr a1, VStr a2 -> String.equal a1 a2
  | _ -> false

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r -> VInt (l / r)
  | Add, VInt l, VInt r -> VInt (l + r)
  | Add, VStr l, VStr r -> VStr (l ^ r)
  | Sub, VInt l, VInt r -> VInt (l - r)
  | Eq, l, r -> VBool (equal_value l r)
  | Neq, l, r -> VBool (equal_value l r |> not)
  | Gt, VInt l, VInt r -> VBool (l > r)
  | Geq, VInt l, VInt r -> VBool (l >= r)
  | Lt, VInt l, VInt r -> VBool (l < r)
  | Leq, VInt l, VInt r -> VBool (l <= r)
  | _ -> failwith "type error"

let rec eval (e : expr) (m : memory) : value =
  match e with
  | Int a -> VInt a
  | Bool b -> VBool b
  | Str s -> VStr s
  | Binop (op, l, r) -> eval_op op (eval l m) (eval r m)
  | Var x ->
      begin match Memory.find_opt x m with
      | Some v -> v
      | None -> VInt 0 (* wartość domyślna *)
      end

let rec exec (s : stmt) (m : memory) : memory =
  match s with
  | Assign (x, e) ->
      let v = eval e m in
      Memory.add x v m
  | Skip -> m
  | Cmp (s1, s2) ->
      m |> exec s1 |> exec s2
  | Print e ->
      let v = eval e m in
      print_value v;
      m
  | If (p, t, e) ->
      let v = eval p m in
      (match v with
       | VBool true -> exec t m
       | VBool false -> exec e m
       | _ -> failwith "type error")
  | While (p, b) ->
      let v = eval p m in
      (match v with
       | VBool false -> m
       | VBool true -> exec b m |> exec s
       | _ -> failwith "type error")

let get_prio = function
  | Mult -> 2
  | Div -> 2
  | Add -> 1
  | Sub -> 1
  | _ -> 0

let rec pretty_print_expr (root_op_prio : int) = function
  | Int a -> string_of_int a
  | Bool b -> if b then "true" else "false"
  | Str s -> "\"" ^ String.escaped s ^ "\""
  | Binop (op, l, r) ->
    let op_s = match op with
     | Mult -> "*"
     | Div -> "/"
     | Add -> "+"
     | Sub -> "-"
     | Eq -> "=="
     | Neq -> "<>"
     | Gt -> ">"
     | Geq -> ">="
     | Lt -> "<"
     | Leq -> "<="
    in
    let prio = get_prio op in
    let delta = prio - root_op_prio in
    let wrap s = if delta >= 0 then s else "(" ^ s ^ ")" in
    wrap (pretty_print_expr prio l ^ op_s ^ pretty_print_expr (prio + 1) r)
  | Var x -> x

let parse (s : string) : stmt =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : unit =
  ignore (exec (parse s) Memory.empty)


let () =
  (*

  (2 + 3) * x

  *)
  (* let my_expr =
    Ast.Binop(
      Ast.Mult,
      Ast.Binop(Ast.Add, Ast.Int 2, Ast.Int 3),
      Ast.Var "x"
    )
  in *)


  (*

  (2 + 3) * x

  *)
  (* match parse("x = 2 + ((8 + 9) / 42) * 2 / 5;") with *)
  match parse("x = 1 - (2 - 3);") with
    | Cmp(Assign(_,e), _) ->
        let result = pretty_print_expr 0 e in
        print_endline ("Pretty printed: " ^ result)
    | _ -> failwith ""
