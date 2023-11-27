open Ast
(* open Util *)

let rec fold : expr -> expr = function
  | Prim1 (Add1, e) -> (
    match fold e with Num n -> Num (n + 1) | e -> Prim1 (Add1, e) )
  | Prim1 (Sub1, e) -> (
    match fold e with Num n -> Num (n - 1) | e -> Prim1 (Sub1, e) )
  | Prim1 (p, e) ->
      Prim1 (p, fold e)
  | Prim2 (Plus, e1, e2) -> (
    match (fold e1, fold e2) with
    | Num n1, Num n2 ->
        Num (n1 + n2)
    | e1, e2 ->
        Prim2 (Plus, e1, e2) )
  | Prim2 (Minus, e1, e2) -> (
    match (fold e1, fold e2) with
    | Num n1, Num n2 ->
        Num (n1 - n2)
    | e1, e2 ->
        Prim2 (Minus, e1, e2) )
  | Prim2 (p, e1, e2) ->
      Prim2 (p, fold e1, fold e2)
  | e ->
      e

let fold_program (prog : program) =
  { defns=
      List.map
        (fun {name; args; body} -> {name; args; body= fold body})
        prog.defns
  ; body= fold prog.body }
