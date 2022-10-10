open! Core

type register = Register of int

type instruction =
  | Add of register * register
  | Sub of register * register

type registers = int list

type alu_opcode =
  | Add
  | Sub

let decode : instruction -> alu_opcode * register * register = function
  | Add (r1, r2) -> Add, r1, r2
  | Sub (r1, r2) -> Sub, r1, r2
;;

let register_fetch (registers : registers) (Register r1, Register r2) : int * int =
  let nth l i = Option.value ~default:0 (List.nth l i) in
  nth registers r1, nth registers r2
;;

let alu (alu_opcode : alu_opcode) ((i1, i2) : int * int) : int =
  match alu_opcode with
  | Add -> i1 + i2
  | Sub -> i1 - i2
;;

type program = instruction list

let eval_program : program -> registers -> registers =
  List.fold_left ~init:Fn.id ~f:(fun eval_previous instruction registers ->
    let registers' = eval_previous registers in
    let alu_opcode, r1, r2 = decode instruction in
    let inputs = register_fetch registers' (r1, r2) in
    let output = alu alu_opcode inputs in
    output :: registers')
;;

let test () =
  eval_program
    [ Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ; Add (Register 0, Register 1)
    ]
    [ 1; 1 ]
;;
