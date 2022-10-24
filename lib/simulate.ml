open! Core

let ( >>> ) f g x = g (f x)

module RegisterArray = struct
  type t = int list [@@deriving show]
end

module Instruction = struct
  type 'a t =
    | Const of int
    | Add of 'a * 'a
    | Sub of 'a * 'a
  [@@deriving show, map]

  let alu : int t -> int = function
    | Const i -> i
    | Add (i1, i2) -> i1 + i2
    | Sub (i1, i2) -> i1 - i2
  ;;
end

module type REGISTER = sig
  type t

  val fetch : RegisterArray.t -> t -> int
end

module Processor (Register : REGISTER) = struct
  let eval_instruction (registers : RegisterArray.t) : Register.t Instruction.t -> int =
    Instruction.map (Register.fetch registers) >>> Instruction.alu
  ;;

  type program = Register.t Instruction.t list * int

  let cycle_program : program * RegisterArray.t -> program option * RegisterArray.t =
   fun ((program, instruction_pointer), registers) ->
    match List.nth program instruction_pointer with
    | None -> None, registers
    | Some instruction ->
      let output = eval_instruction registers instruction in
      Some (program, instruction_pointer + 1), output :: registers
 ;;

  let rec eval_program (program : program) (registers : RegisterArray.t) : RegisterArray.t
    =
    match cycle_program (program, registers) with
    | None, registers' -> registers'
    | Some program', registers' -> eval_program program' registers'
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
