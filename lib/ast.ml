open S_exp

type prim0 = ReadNum | NewLIne

let prim0_of_string (s : string) : prim0 option =
  match s with
  | "read-num" ->
      Some ReadNum
  | "newline" ->
      Some NewLIne
  | _ ->
      None

type prim1 = Add1 | Sub1 | ZeroP | NumP | Not | Left | Right | Print

let prim1_of_string (s : string) : prim1 option =
  match s with
  | "add1" ->
      Some Add1
  | "sub1" ->
      Some Sub1
  | "zero?" ->
      Some ZeroP
  | "num?" ->
      Some NumP
  | "not" ->
      Some Not
  | "left" ->
      Some Left
  | "right" ->
      Some Right
  | "print" ->
      Some Print
  | _ ->
      None

type prim2 = Plus | Minus | Eq | Lt | Pair

let prim2_of_string = function
  | "+" ->
      Some Plus
  | "-" ->
      Some Minus
  | "=" ->
      Some Eq
  | "<" ->
      Some Lt
  | "pair" ->
      Some Pair
  | _ ->
      None

type expr =
  | True
  | False
  | Num of int
  | Var of string
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Do of expr list
  | Call of expr * expr list

exception BadSExpression of s_exp

type defn = {name: string; args: string list; body: expr}

type program = {defns: defn list; body: expr}

let is_defn defns name = List.exists (fun d -> d.name = name) defns

let get_defn defns name = List.find (fun d -> d.name = name) defns

let rec expr_of_s_exp (e : s_exp) : expr =
  match e with
  | Num n ->
      Num n
  | Sym "true" ->
      True
  | Sym "false" ->
      False
  | Sym var ->
      Var var
  | Lst [Sym "if"; e1; e2; e3] ->
      If (expr_of_s_exp e1, expr_of_s_exp e2, expr_of_s_exp e3)
  | Lst [Sym "let"; Lst [Lst [Sym s; e]]; body] ->
      Let (s, expr_of_s_exp e, expr_of_s_exp body)
  | Lst [Sym op] when Option.is_some (prim0_of_string op) ->
      Prim0 (Option.get (prim0_of_string op))
  | Lst [Sym op; e1] when Option.is_some (prim1_of_string op) ->
      Prim1 (Option.get (prim1_of_string op), expr_of_s_exp e1)
  | Lst [Sym op; e1; e2] when Option.is_some (prim2_of_string op) ->
      Prim2
        ( Option.get (prim2_of_string op)
        , expr_of_s_exp e1
        , expr_of_s_exp e2 )
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_of_s_exp exps)
  | Lst (f :: args) ->
      Call (expr_of_s_exp f, List.map expr_of_s_exp args)
  | _ ->
      raise (BadSExpression e)

let program_of_s_exps (exps : s_exp list) : program =
  let rec get_args args =
    match args with
    | Sym v :: args ->
        v :: get_args args
    | e :: _ ->
        raise (BadSExpression e)
    | [] ->
        []
  in
  let get_defn = function
    | Lst [Sym "define"; Lst (Sym name :: args); body] ->
        let args = get_args args in
        {name; args; body= expr_of_s_exp body}
    | e ->
        raise (BadSExpression e)
  in
  let rec go exps defns =
    match exps with
    | [e] ->
        {defns= List.rev defns; body= expr_of_s_exp e}
    | d :: exps ->
        go exps (get_defn d :: defns)
    | _ ->
        raise (BadSExpression (Sym "empty"))
  in
  go exps []
