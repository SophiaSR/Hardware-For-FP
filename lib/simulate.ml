open! Core

type register = Register of int

type 'a instruction =
  | Const of int
  | Add of 'a * 'a
  | Sub of 'a * 'a
[@@deriving map]

type registers = int list

type alu_opcode =
  | Add
  | Sub

let register_fetch (registers : registers) (Register r) : int =
  Option.value ~default:0 (List.nth registers r)
;;

let alu : int instruction -> int = function
  | Const i -> i
  | Add (i1, i2) -> i1 + i2
  | Sub (i1, i2) -> i1 - i2
;;

let eval_instruction (instruction : register instruction) (registers : registers) : int =
  instruction |> map_instruction (register_fetch registers) |> alu
;;

type program = register instruction list

let eval_program : program -> registers -> registers =
  List.fold_left ~init:Fn.id ~f:(fun eval_previous instruction registers ->
    let registers' = eval_previous registers in
    let output = eval_instruction instruction registers' in
    output :: registers')
;;

let test () =
  eval_program
    [ Const 0
    ; Const 1
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ]
    []
;;
