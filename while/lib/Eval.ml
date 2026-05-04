open Ast


module Array : sig
  type 'a t
  val empty: 'a t
  val add : int -> 'a -> 'a t -> 'a t
  val find_opt : int -> 'a t -> 'a option
  val bindings : 'a t -> (int * 'a) list
  val of_list : (int * 'a) list -> 'a t
end = Map.Make(Int)

type value =
  | VInt of Bigint.t
  | VBool of bool
  | VStr of string
  | VArray of value Array.t


module Memory : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type memory = value Memory.t


let rec show_value (v : value) : string =
  match v with
  | VInt a -> Bigint.to_string a
  | VBool b -> string_of_bool b
  | VStr s -> s
  (* | VArray a -> "{" ^ (Array.fold (fun key (v: value) acc -> ((show_value v) ^ ", " ^ acc)) a "") ^ "}" *)
  | VArray a ->
  let show_value_nested (v : value) : string =
    (    match v with
    | VStr s -> "\"" ^ s ^ "\""
    | v -> show_value v)
  in
  "{" ^ (let l = Array.bindings a |> List.map (snd) in List.fold_right (fun (v) acc -> (let join = match acc with "" -> "" | _ -> "; " in (show_value_nested v) ^ join ^ acc)) l "") ^ "}"

let print_value (v : value) : unit =
  v |> show_value |> print_string

let rec equal_value (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VInt a1, VInt a2 -> Bigint.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VStr a1, VStr a2 -> String.equal a1 a2
  | _ -> false

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (Bigint.(l * r))
  | Div, VInt l, VInt r -> VInt (Bigint.(l / r))
  | Add, VInt l, VInt r -> VInt (Bigint.(l + r))
  | Add, VStr l, VStr r -> VStr (l ^ r)
  | Sub, VInt l, VInt r -> VInt (Bigint.(l - r))
  | Eq, l, r -> VBool (equal_value l r)
  | Neq, l, r -> VBool (equal_value l r |> not)
  | Gt, VInt l, VInt r -> VBool (Bigint.(l > r))
  | Geq, VInt l, VInt r -> VBool (Bigint.(l >= r))
  | Lt, VInt l, VInt r -> VBool (Bigint.(l < r))
  | Leq, VInt l, VInt r -> VBool (Bigint.(l <= r))
  | _ -> failwith "type error"

let rec eval (e : expr) (m : memory) : value =
  match e with
  | Int a -> VInt a
  | Bool b -> VBool b
  | Str s -> VStr s
  | Binop (op, l, r) -> eval_op op (eval l m) (eval r m)
  | Array a ->
      let arr_map, _ = List.fold_left (fun (acc, i) e -> (Array.add i (eval e m) acc, i + 1)) (Array.empty, 0) a in
      VArray arr_map
  | Var x ->
      begin match Memory.find_opt x m with
      | Some v -> v
      | None -> VInt Bigint.zero (* wartość domyślna *)
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
  | Int a -> Bigint.to_string a
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
  | Array a -> "{" ^ (List.fold_right (fun v acc -> (let join = match acc with "" -> "" | _ -> "; " in (pretty_print_expr 0 v) ^ join ^ acc)) a "") ^ "}"

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
  ignore (exec (parse s) Memory.empty)
