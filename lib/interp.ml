open S_exp
open Ast_lam
open Util

exception BadExpression of expr

type value =
  | Number of int
  | Boolean of bool
  | Pair of value * value
  | Function of string * value symtab

let rec string_of_value (v : value) : string =
  match v with
  | Number n ->
      string_of_int n
  | Boolean b ->
      if b then "true" else "false"
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_value v1)
        (string_of_value v2)
  | Function _ ->
      "<function>"

let input_channel : in_channel ref = ref stdin

let output_channel : out_channel ref = ref stdout

let rec interp_exp (defns : defn list) (env : value symtab)
    (exp : expr) : value =
  match exp with
  | Num n ->
      Number n
  | True ->
      Boolean true
  | False ->
      Boolean false
  | Var var when Symtab.mem var env ->
      Symtab.find var env
  | Var var when is_defn defns var ->
      Function (var, Symtab.empty)
  | Var _ ->
      raise (BadExpression exp)
  | Closure f ->
      Function (f, env)
  | Prim0 ReadNum ->
      Number (input_line !input_channel |> int_of_string)
  | Prim0 NewLIne ->
      output_string !output_channel "\n" ;
      Boolean true
  | Prim1 (Print, e) ->
      interp_exp defns env e |> string_of_value
      |> output_string !output_channel ;
      Boolean true
  | Prim1 (Not, arg) ->
      if interp_exp defns env arg = Boolean false then Boolean true
      else Boolean false
  | Prim1 (ZeroP, arg) -> (
    match interp_exp defns env arg with
    | Number 0 ->
        Boolean true
    | _ ->
        Boolean false )
  | Prim1 (NumP, arg) -> (
    match interp_exp defns env arg with
    | Number _ ->
        Boolean true
    | _ ->
        Boolean false )
  | Prim1 (Add1, arg) -> (
    match interp_exp defns env arg with
    | Number n ->
        Number (n + 1)
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Sub1, arg) -> (
    match interp_exp defns env arg with
    | Number n ->
        Number (n - 1)
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Left, e) -> (
    match interp_exp defns env e with
    | Pair (l, _) ->
        l
    | _ ->
        raise (BadExpression exp) )
  | Prim1 (Right, e) -> (
    match interp_exp defns env e with
    | Pair (_, r) ->
        r
    | _ ->
        raise (BadExpression exp) )
  | Prim2 (Plus, e1, e2) -> (
      let lhs = interp_exp defns env e1 in
      let rhs = interp_exp defns env e2 in
      match (lhs, rhs) with
      | Number n1, Number n2 ->
          Number (n1 + n2)
      | _ ->
          raise (BadExpression exp) )
  | Prim2 (Minus, e1, e2) -> (
      let lhs = interp_exp defns env e1 in
      let rhs = interp_exp defns env e2 in
      match (lhs, rhs) with
      | Number n1, Number n2 ->
          Number (n1 - n2)
      | _ ->
          raise (BadExpression exp) )
  | Prim2 (Eq, e1, e2) -> (
      let lhs = interp_exp defns env e1 in
      let rhs = interp_exp defns env e2 in
      match (lhs, rhs) with
      | Number n1, Number n2 ->
          Boolean (n1 = n2)
      | Boolean b1, Boolean b2 ->
          Boolean (b1 = b2)
      | _ ->
          raise (BadExpression exp) )
  | Prim2 (Lt, e1, e2) -> (
      let lhs = interp_exp defns env e1 in
      let rhs = interp_exp defns env e2 in
      match (lhs, rhs) with
      | Number n1, Number n2 ->
          Boolean (n1 < n2)
      | _ ->
          raise (BadExpression exp) )
  | Prim2 (Pair, e1, e2) ->
      let lhs = interp_exp defns env e1 in
      let rhs = interp_exp defns env e2 in
      Pair (lhs, rhs)
  | Let (var, e, body) ->
      let e_val = interp_exp defns env e in
      interp_exp defns (Symtab.add var e_val env) body
  | If (test_exp, then_exp, else_exp) ->
      if interp_exp defns env test_exp = Boolean false then
        interp_exp defns env else_exp
      else interp_exp defns env then_exp
  | Do exps ->
      exps |> List.rev_map (interp_exp defns env) |> List.hd
  | Call (f, args) -> (
      let vals = List.map (interp_exp defns env) args in
      let fv = interp_exp defns env f in
      match fv with
      | Function (f, saved_env) when is_defn defns f ->
          let defn = get_defn defns f in
          if List.length args <> List.length defn.args then
            raise (BadExpression exp)
          else
            let fenv =
              List.combine defn.args vals |> Symtab.add_list saved_env
            in
            interp_exp defns fenv defn.body
      | _ ->
          raise (BadExpression exp) )

let interp (program : string) : unit =
  let program2 = parse_many program |> program_of_s_exps in
  interp_exp program2.defns Symtab.empty program2.body |> ignore

let interp_io (program : string) (input : string) =
  let input_pipe_ex, input_pipe_en = Unix.pipe () in
  let output_pipe_ex, output_pipe_en = Unix.pipe () in
  input_channel := Unix.in_channel_of_descr input_pipe_ex ;
  set_binary_mode_in !input_channel false ;
  output_channel := Unix.out_channel_of_descr output_pipe_en ;
  set_binary_mode_out !output_channel false ;
  let write_input_channel = Unix.out_channel_of_descr input_pipe_en in
  set_binary_mode_out write_input_channel false ;
  let read_output_channel = Unix.in_channel_of_descr output_pipe_ex in
  set_binary_mode_in read_output_channel false ;
  output_string write_input_channel input ;
  close_out write_input_channel ;
  interp program ;
  close_out !output_channel ;
  let r = input_all read_output_channel in
  input_channel := stdin ;
  output_channel := stdout ;
  r

let interp_err (program : string) (input : string) : string =
  try interp_io program input with BadExpression _ -> "ERROR"
