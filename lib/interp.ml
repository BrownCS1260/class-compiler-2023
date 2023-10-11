open S_exp
open Ast
open Util

exception BadExpression of expr

type value = Number of int | Boolean of bool | Pair of value * value

let rec string_of_value (v : value) : string =
  match v with
  | Number n ->
      string_of_int n
  | Boolean b ->
      if b then "true" else "false"
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_value v1)
        (string_of_value v2)

let rec interp_exp (env : value symtab) (exp : expr) : value =
  match exp with
  | Num n ->
      Number n
  | True ->
      Boolean true
  | False ->
      Boolean false
  | Var var when Symtab.mem var env ->
      Symtab.find var env
  | Var _ ->
      raise (BadExpression exp)
  | Prim1 (Not, arg) ->
      if interp_exp env arg = Boolean false then Boolean true
      else Boolean false
  | Prim1 (ZeroP, arg) -> (
    match interp_exp env arg with
    | Number 0 ->
        Boolean true
    | _ ->
        Boolean false )
  | Prim1 (NumP, arg) -> (
    match interp_exp env arg with
    | Number _ ->
        Boolean true
    | _ ->
        Boolean false )
  | Prim1 (Add1, arg) -> (
    match interp_exp env arg with
    | Number n ->
        Number (n + 1)
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Sub1, arg) -> (
    match interp_exp env arg with
    | Number n ->
        Number (n - 1)
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Left, e) -> (
    match interp_exp env e with
    | Pair (l, _) ->
        l
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Right, e) -> (
    match interp_exp env e with
    | Pair (_, r) ->
        r
    | _ ->
        raise (BadExpression exp) )
  | Prim2 (Plus, e1, e2) -> (
    match (interp_exp env e1, interp_exp env e2) with
    | Number n1, Number n2 ->
        Number (n1 + n2)
    | _ ->
        raise (BadExpression exp) )
  | Prim2 (Minus, e1, e2) -> (
    match (interp_exp env e1, interp_exp env e2) with
    | Number n1, Number n2 ->
        Number (n1 - n2)
    | _ ->
        raise (BadExpression exp) )
  | Prim2 (Eq, e1, e2) ->
      Boolean (interp_exp env e1 = interp_exp env e2)
  | Prim2 (Lt, e1, e2) -> (
    match (interp_exp env e1, interp_exp env e2) with
    | Number n1, Number n2 ->
        Boolean (n1 < n2)
    | _ ->
        raise (BadExpression exp) )
  | Prim2 (Pair, e1, e2) ->
      Pair (interp_exp env e1, interp_exp env e2)
  | Let (var, e, body) ->
      let e_val = interp_exp env e in
      interp_exp (Symtab.add var e_val env) body
  | If (test_exp, then_exp, else_exp) ->
      if interp_exp env test_exp = Boolean false then
        interp_exp env else_exp
      else interp_exp env then_exp

let interp (program : string) : string =
  parse program |> expr_of_s_exp |> interp_exp Symtab.empty
  |> string_of_value
