open! Core

module RegisterArray = struct
  type t = int list [@@deriving show]
end

module Instruction = struct
  type 'a t =
    | Const of int
    | Add of 'a * 'a
    | Sub of 'a * 'a
  [@@deriving show, map]
end

module type REGISTER = sig
  type t

  val fetch : RegisterArray.t -> t -> int
end

module Processor (Register : REGISTER) = struct
  let alu : int Instruction.t -> int = function
    | Const i -> i
    | Add (i1, i2) -> i1 + i2
    | Sub (i1, i2) -> i1 - i2
  ;;

  let eval_instruction
    (instruction : Register.t Instruction.t)
    (registers : RegisterArray.t)
    : int
    =
    instruction |> Instruction.map (Register.fetch registers) |> alu
  ;;

  type program = Register.t Instruction.t list

  let eval_program : program -> RegisterArray.t -> RegisterArray.t =
    List.fold_left ~init:Fn.id ~f:(fun eval_previous instruction registers ->
      let registers' = eval_previous registers in
      let output = eval_instruction instruction registers' in
      output :: registers')
  ;;
end

module Register = struct
  type t = int [@@deriving show]

  let fetch (registers : RegisterArray.t) (r : t) : int =
    Option.value ~default:0 (List.nth registers r)
  ;;
end

module Source = struct
  type t =
    | Literal of int
    | Register of Register.t
  [@@deriving show]

  let fetch (registers : RegisterArray.t) : t -> int = function
    | Literal i -> i
    | Register r -> Register.fetch registers r
  ;;
end
