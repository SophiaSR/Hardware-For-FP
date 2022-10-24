open! Core

let ( >>> ) f g x = g (f x)

module RegisterArray = struct
  type t = int list [@@deriving show]
end

module Effect = struct
  type state = int
  type 'a t = state -> 'a * state

  let return : 'a -> 'a t = fun a s -> a, s

  let bind : 'a t * ('a -> 'b t) -> 'b t =
   fun (at, f) s ->
    let a, s' = at s in
    f a s'
 ;;

  module Let_syntax = struct
    let return = return
    let bind x ~f = bind (x, f)
  end
end

module Instruction = struct
  type 'a t =
    | Const of int
    | Add of 'a * 'a
    | Sub of 'a * 'a
    | Load
    | Store of 'a
  [@@deriving show, map]

  let alu : int t -> int Effect.t = function
    | Const i -> Effect.return i
    | Add (i1, i2) -> Effect.return (i1 + i2)
    | Sub (i1, i2) -> Effect.return (i1 - i2)
    | Load -> fun s -> s, s
    | Store i -> fun s -> s, i
  ;;
end

module type REGISTER = sig
  type t

  val fetch : RegisterArray.t -> t -> int
end

module Processor (Register : REGISTER) = struct
  let eval_instruction (registers : RegisterArray.t)
    : Register.t Instruction.t -> int Effect.t
    =
    Instruction.map (Register.fetch registers) >>> Instruction.alu
  ;;

  type program = Register.t Instruction.t list * int

  let cycle_program
    : program * RegisterArray.t -> (program option * RegisterArray.t) Effect.t
    =
   fun ((program, instruction_pointer), registers) ->
    Effect.(
      match List.nth program instruction_pointer with
      | None -> return (None, registers)
      | Some instruction ->
        let%bind output = eval_instruction registers instruction in
        return (Some (program, instruction_pointer + 1), output :: registers))
 ;;

  let rec eval_program (program : program) (registers : RegisterArray.t)
    : RegisterArray.t Effect.t
    =
    Effect.(
      match%bind cycle_program (program, registers) with
      | None, registers' -> return registers'
      | Some program', registers' -> eval_program program' registers')
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
