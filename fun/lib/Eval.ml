open Ast

module Env : sig
  type 'a t

  val empty : 'a t
  val add : ident -> 'a -> 'a t -> 'a t
  val mem : ident -> 'a t -> bool
  val find_opt : ident -> 'a t -> 'a option
end = struct
  module M = Map.Make (String)

  type 'a t = 'a M.t

  let empty = M.empty
  let add k v env = match k with Ident s -> M.add s v env | Wildcard -> env
  let mem k env = match k with Ident s -> M.mem s env | Wildcard -> false

  let find_opt k env =
    match k with Ident s -> M.find_opt s env | Wildcard -> None
end

type env = value Env.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VClosure of env * ident * expr
  | VRecClosure of env * ident * ident * expr

let rec show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VPair (v1, v2) -> Printf.sprintf "(%s,%s)" (show_value v1) (show_value v2)
  | VClosure _ | VRecClosure _ -> "<fun>"

let print_value (v : value) : unit = v |> show_value |> print_endline

let rec equal_value (v1 : value) (v2 : value) : bool =
  match (v1, v2) with
  | VInt a1, VInt a2 -> Int.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VUnit, VUnit -> true
  | VPair (a1, a2), VPair (b1, b2) -> equal_value a1 b1 && equal_value a2 b2
  | _ -> false

let eval_op (op : bop) (l : value) (r : value) : value =
  match (op, l, r) with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r -> VInt (l / r)
  | Add, VInt l, VInt r -> VInt (l + r)
  | Sub, VInt l, VInt r -> VInt (l - r)
  | Eq, l, r -> VBool (equal_value l r)
  | Neq, l, r -> VBool (equal_value l r |> not)
  | Gt, VInt l, VInt r -> VBool (l > r)
  | Geq, VInt l, VInt r -> VBool (l >= r)
  | Lt, VInt l, VInt r -> VBool (l < r)
  | Leq, VInt l, VInt r -> VBool (l <= r)
  | _ -> failwith "type error"

let free_vars env defs =
  let rec in_env env expr acc =
    match expr with
    | Var y -> (
        match (y, Env.find_opt y env) with
        | Ident _, None -> y :: acc
        | _ -> acc)
    | Let (x, e1, e2) ->
        in_env (Env.add x VUnit env) e1 (in_env (Env.add x VUnit env) e2 acc)
    | Match (e1, ids, e2) ->
        let e = List.fold_left (fun acc x -> Env.add x VUnit acc) env ids in
        in_env e e1 (in_env e e2 acc)
    | Binop (_, l, r) | If (_, l, r) | Pair (l, r) ->
        in_env env l (in_env env r acc)
    | Fun (x, e) -> in_env (Env.add x VUnit env) e acc
    | Funrec (f, x, e) ->
        in_env (env |> Env.add x VUnit |> Env.add f VUnit) e acc
    | _ -> acc
  in
  let rec in_defs env xs acc =
    match xs with
    | [] -> acc
    | (i, e) :: xxs -> in_defs (Env.add i VUnit env) xxs (in_env env e [])
  in
  in_defs env defs []

let check_unused_expr expr =
  let rec uses id expr =
    match expr with
    | Var y -> y = id
    | Let (x, e1, e2) -> uses id e1 || (x <> id && uses id e2)
    | Match (e1, ids, e2) ->
        uses id e1 || ((not (List.mem id ids)) && uses id e2)
    | Binop (_, l, r) | Pair (l, r) | App (l, r) -> uses id l || uses id r
    | If (e, l, r) -> uses id e || uses id l || uses id r
    | Fun (x, e) -> x <> id && uses id e
    | Funrec (f, x, e) -> f <> id && x <> id && uses id e
    | _ -> false
  in
  let rec collect expr acc =
    match expr with
    | Let ((Ident x as id), e1, e2) ->
        let acc' = if not (uses id e2) then x :: acc else acc in
        collect e1 (collect e2 acc')
    | Match (e1, ids, e2) ->
        let acc' =
          List.fold_left
            (fun a -> function
              | Ident x as id -> if not (uses id e2) then x :: a else a
              | Wildcard -> a)
            acc ids
        in
        collect e1 (collect e2 acc')
    | Fun ((Ident x as id), e) ->
        let acc' = if not (uses id e) then x :: acc else acc in
        collect e acc'
    | Funrec ((Ident f as idf), (Ident x as idx), e) ->
        let acc' =
          (acc |> fun a -> if not (uses idf e) then f :: a else a) |> fun a ->
          if not (uses idx e) then x :: a else a
        in
        collect e acc'
    | Let (Wildcard, e1, e2) -> collect e1 (collect e2 acc)
    | Fun (Wildcard, e) -> collect e acc
    | Funrec (Wildcard, _, e) -> collect e acc
    | Binop (_, l, r) | If (_, l, r) | Pair (l, r) | App (l, r) ->
        collect l (collect r acc)
    | _ -> acc
  in
  collect expr []

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int a -> VInt a
  | Binop (op, l, r) -> eval_op op (eval env l) (eval env r)
  | Bool b -> VBool b
  | If (b, t, e) -> (
      match eval env b with
      | VBool true -> eval env t
      | VBool false -> eval env e
      | _ -> failwith "type error")
  | Unit -> VUnit
  | Pair (e1, e2) -> VPair (eval env e1, eval env e2)
  | Match (e1, ids, e2) -> (
      match eval env e1 with
      | VPair _ as pair_val ->
          (* Recursywnie przypisujemy identyfikatory do kolejnych struktur pary *)
          let rec bind_vars ids v current_env =
            match (ids, v) with
            | [], _ -> failwith "match error: no identifiers provided"
            | [ x ], _ ->
                (* Ostatni identyfikator jest przypisany do ogona krotki *)
                Env.add x v current_env
            | x :: xs, VPair (v1, v2) ->
                (* Przypisz x do lewej strony i kontynuuj odpakowywanie prawej *)
                let env' = Env.add x v1 current_env in
                bind_vars xs v2 env'
            | _ -> failwith "match error: not enough elements in tuple to bind"
          in
          let env' = bind_vars ids pair_val env in
          eval env' e2
      | _ -> failwith "type error: expected a pair to unpack")
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.add x v1 env in
      let v2 = eval env' e2 in
      v2
  | Var y -> (
      match Env.find_opt y env with
      | Some v -> v
      | None -> (
          match y with
          | Ident s -> failwith ("unknown var " ^ s)
          | Wildcard -> failwith "wildcard used as a variable"))
  | Fun (x, e) -> VClosure (env, x, e)
  | Funrec (f, x, e) -> VRecClosure (env, f, x, e)
  | App (e1, e2) -> (
      match eval env e1 with
      | VClosure (env', x, body) ->
          let v_arg = eval env e2 in
          eval (Env.add x v_arg env') body
      | VRecClosure (env', f, x, body) as c ->
          let v_arg = eval env e2 in
          let env'' = env' |> Env.add x v_arg |> Env.add f c in
          eval env'' body
      | _ -> failwith "type error")

let interp (env : env) (s : string) : value =
  let ast = Parser.main Lexer.read (Lexing.from_string s) in
  eval env ast

let eval_def (env : env) ((x, e) : def) : env =
  let print = function Ident s -> print_endline s | Wildcard -> () in
  let v = eval env e in
  print x;
  Env.add x v env

let eval_prog (env : env) (s : def list) : env = List.fold_left eval_def env s

let interp_prog (env : env) (s : string) : env =
  let defs = Parser.program Lexer.read (Lexing.from_string s) in
  match free_vars env defs |> List.filter (fun id -> not (Env.mem id env)) with
  | [] ->
      List.iter
        (fun (id, expr) ->
          let unused = check_unused_expr expr in
          List.iter
            (fun x -> Printf.printf "Warning: unused variable %s\n" x)
            unused)
        defs;
      eval_prog env defs
  | xs ->
      failwith
        ("Unused variables! "
        ^ List.fold_left
            (fun acc x ->
              (match x with Ident s -> s ^ ", " | Wildcard -> "") ^ acc)
            "" xs)
