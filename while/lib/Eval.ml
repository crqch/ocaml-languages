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



type halting =
  | Normal
  | Halted

let rec exec (s : stmt) (m : memory): memory * halting =
  match s with
  | Assign (x, e) ->
      let v = eval e m in
      (Memory.add x v m, Normal)
  | Skip -> (m, Normal)
  | Cmp (s1, s2) ->
      (match m |> exec s1 with
      | (m, Halted) as t -> t
      | (m, Normal) -> exec s2 m)
  | Print e ->
      let v = eval e m in
      print_value v;
      (m,Normal)
  | If (p, t, e) ->
      let v = eval p m in
      (match v with
       | VBool true -> exec t m
       | VBool false -> exec e m
       | _ -> failwith "type error")
  | While (p, b) ->
      let v = eval p m in
      (match v with
       | VBool false -> (m,Normal)
       | VBool true -> (match exec b m with
         | (m, Halted) as t -> t
         | (m, Normal) -> exec s m)
       | _ -> failwith "type error")
  | Halt -> (m, Halted)

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

let rec tab_repeat i =
  if i > 0 then "  " ^ tab_repeat (i-1) else ""

let rec pretty_print (level: int) stmt =
  let tabs = tab_repeat(level) in
  match stmt with
  | Assign (x, e) -> tabs ^ x ^ " = " ^ pretty_print_expr 0 e ^ ";\n"
  | Skip -> tabs ^ "skip;\n"
  | Cmp (s1, s2) ->
    if level == 0 then
   "{\n" ^ pretty_print 1 s1 ^ "\n" ^ pretty_print 1 s2 ^ "}\n"
   else
    (pretty_print level s1) ^ (pretty_print level s2)
  | Print e ->
    tabs ^ "print(" ^ (pretty_print_expr 0 e) ^ ");\n"
  | If (p, t, e) ->
    tabs ^ "if (" ^ ( pretty_print_expr 0 p) ^ ") {\n" ^ (pretty_print (level+1) t)  ^ tabs ^ "} else {\n" ^ (pretty_print (level+1) e) ^ tabs ^ "}\n"
  | While (p, b) ->
    tabs ^ "while (" ^ (pretty_print_expr 0 p) ^ ") {\n" ^ (pretty_print (level+1) b) ^ tabs ^ "}"
  | Halt -> tabs ^ "halt;\n"

let parse (s : string) : stmt =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : unit =
  let ast = parse s in
  let s1 = pretty_print 0 ast in
  print_string s1;
  ignore (exec (parse s1) Memory.empty)
