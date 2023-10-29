open S_exp
open Asm
open Util
open Ast

exception BadExpression of expr

let num_shift = 2

let num_mask = 0b11

let num_tag = 0b00

let bool_shift = 7

let bool_mask = 0b1111111

let bool_tag = 0b0011111

let heap_mask = 0b111

let pair_tag = 0b010

let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (x : int) : operand =
  Imm ((x lsl num_shift) lor num_tag)

let zf_to_bool : directive list =
  [ Mov (Reg Rax, Imm 0)
  ; Setz (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

let lf_to_bool : directive list =
  [ Mov (Reg Rax, Imm 0)
  ; Setl (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

(* overwrites r8! *)
let ensure_num (op : operand) : directive list =
  [ Mov (Reg R8, op)
  ; And (Reg R8, Imm num_mask)
  ; Cmp (Reg R8, Imm num_tag)
  ; Jnz "error" ]

(* overwrites r8! *)
let ensure_pair (op : operand) : directive list =
  [ Mov (Reg R8, op)
  ; And (Reg R8, Imm heap_mask)
  ; Cmp (Reg R8, Imm pair_tag)
  ; Jnz "error" ]

let stack_address (stack_index : int) =
  MemOffset (Reg Rsp, Imm stack_index)

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

let rec compile_exp (tab : int symtab) (stack_index : int) (exp : expr)
    : directive list =
  match exp with
  | True ->
      [Mov (Reg Rax, operand_of_bool true)]
  | False ->
      [Mov (Reg Rax, operand_of_bool false)]
  | Num n ->
      [Mov (Reg Rax, operand_of_num n)]
  | Var var when Symtab.mem var tab ->
      [Mov (Reg Rax, stack_address (Symtab.find var tab))]
  | Var _ ->
      raise (BadExpression exp)
  | Prim0 ReadNum ->
      [ Mov (stack_address stack_index, Reg Rdi)
      ; Add (Reg Rsp, Imm (align_stack_index stack_index))
      ; Call "read_num"
      ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
      ; Mov (Reg Rdi, stack_address stack_index) ]
  | Prim0 NewLIne ->
      [ Mov (stack_address stack_index, Reg Rdi)
      ; Add (Reg Rsp, Imm (align_stack_index stack_index))
      ; Call "print_newline"
      ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
      ; Mov (Reg Rdi, stack_address stack_index)
      ; Mov (Reg Rax, operand_of_bool true) ]
  | Prim1 (Print, e) ->
      compile_exp tab stack_index e
      @ [ Mov (stack_address stack_index, Reg Rdi)
        ; Mov (Reg Rdi, Reg Rax)
        ; Add (Reg Rsp, Imm (align_stack_index stack_index))
        ; Call "print_value"
        ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
        ; Mov (Reg Rdi, stack_address stack_index)
        ; Mov (Reg Rax, operand_of_bool true) ]
  | Prim1 (Add1, arg) ->
      compile_exp tab stack_index arg
      @ [Mov (Reg R8, Reg Rax)]
      @ ensure_num (Reg Rax)
      @ [Add (Reg Rax, Imm (1 lsl num_shift))]
  | Prim1 (Sub1, arg) ->
      compile_exp tab stack_index arg
      @ [Sub (Reg Rax, Imm (1 lsl num_shift))]
  | Prim1 (Not, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, operand_of_bool false)]
      @ zf_to_bool
  | Prim1 (ZeroP, arg) ->
      compile_exp tab stack_index arg
      @ [Cmp (Reg Rax, operand_of_num 0)]
      @ zf_to_bool
  | Prim1 (NumP, arg) ->
      compile_exp tab stack_index arg
      @ [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)]
      @ zf_to_bool
  | Prim1 (Left, arg) ->
      compile_exp tab stack_index arg
      @ ensure_pair (Reg Rax)
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag)))]
  | Prim1 (Right, arg) ->
      compile_exp tab stack_index arg
      @ ensure_pair (Reg Rax)
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8)))]
  | Prim2 (Plus, e1, e2) ->
      compile_exp tab stack_index e1
      @ ensure_num (Reg Rax)
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ ensure_num (Reg Rax)
      @ [Add (Reg Rax, stack_address stack_index)]
  | Prim2 (Minus, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, Reg Rax)
        ; Mov (Reg Rax, stack_address stack_index) ]
      @ [Sub (Reg Rax, Reg R8)]
  | Prim2 (Eq, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, stack_address stack_index)
        ; Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Prim2 (Lt, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Mov (Reg R8, stack_address stack_index)
        ; Cmp (Reg R8, Reg Rax) ]
      @ lf_to_bool
  | Prim2 (Pair, e1, e2) ->
      compile_exp tab stack_index e1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp tab (stack_index - 8) e2
      @ [Mov (Reg R8, stack_address stack_index)]
      @ [ Mov (MemOffset (Reg Rdi, Imm 0), Reg R8)
        ; Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax)
        ; Mov (Reg Rax, Reg Rdi)
        ; Or (Reg Rax, Imm pair_tag)
        ; Add (Reg Rdi, Imm 16) ]
  | If (test_exp, then_exp, else_exp) ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp tab stack_index test_exp
      @ [Cmp (Reg Rax, operand_of_bool false); Jz else_label]
      @ compile_exp tab stack_index then_exp
      @ [Jmp continue_label] @ [Label else_label]
      @ compile_exp tab stack_index else_exp
      @ [Label continue_label]
  | Let (var, e, body) ->
      compile_exp tab stack_index e
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_exp
          (Symtab.add var stack_index tab)
          (stack_index - 8) body
  | Do exps ->
      List.map (fun exp -> compile_exp tab stack_index exp) exps
      |> List.concat

let compile (program : expr) : string =
  [ Global "entry"
  ; Extern "error"
  ; Extern "read_num"
  ; Extern "print_newline"
  ; Extern "print_value"
  ; Label "entry" ]
  @ compile_exp Symtab.empty (-8) program
  @ [Ret]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : string) : unit =
  let file = open_out "program.s" in
  parse program |> expr_of_s_exp |> compile |> output_string file ;
  close_out file

let compile_and_run (program : string) : unit =
  compile_to_file program ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  ignore (Unix.system "./program")

let compile_and_run_io (program : string) (input : string) : string =
  compile_to_file program ;
  ignore (Unix.system "nasm program.s -f macho64 -o program.o") ;
  ignore (Unix.system "gcc program.o runtime.o -o program") ;
  let inp, outp = Unix.open_process "./program" in
  output_string outp input ;
  close_out outp ;
  let r = input_all inp in
  close_in inp ; r

let compile_and_run_err (program : string) (input : string) : string =
  try compile_and_run_io program input
  with BadExpression _ -> "ERROR"
