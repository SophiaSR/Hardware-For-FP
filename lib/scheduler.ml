open! Core

type program =
  | Return of int
  | Add of (int * int) * (int -> program)
  | Let of program * (int -> program)
  | Parallel of (program * program) * (int * int -> program)

let return x = Return x
let add args = Add (args, return)

let example =
  Parallel
    ( (Parallel ((Return 1, Return 2), add), add (3, 4))
    , fun (x, y) -> Parallel ((add (x, y), add (y, 5)), add) )
;;

module Deque : sig
  type 'a t

  val empty : 'a t
  val push_front : 'a t -> 'a -> 'a t
  val push_back : 'a t -> 'a -> 'a t
  val pop_front : 'a t -> ('a t * 'a) option
  val pop_back : 'a t -> ('a t * 'a) option
end = struct
  type 'a t = 'a list

  let empty = []
  let push_front l x = x :: l
  let push_back l x = l @ [ x ]

  let pop_front = function
    | [] -> None
    | x :: l -> Some (l, x)
  ;;

  let pop_back l = Option.map ~f:(fun (l', x) -> List.rev l', x) (pop_front (List.rev l))
end

module Scheduler (S : sig
  val p : int
end) : sig
  (* state of processees with program loaded in *)
  type t

  val start : program -> t
  val step : t -> t
  val to_string : t -> string
end = struct
  open S

  type processor = (program * (int -> program) Deque.t) option
  type t = processor list

  let start program =
    List.init
      ~f:
        (function
         | 0 -> Some (program, Deque.empty)
         | _ -> None)
      p
  ;;

  let step : t -> t =
    List.map
      ~f:
        (Option.map ~f:(fun (program, deque) ->
           match program with
           | Return a ->
             (match Deque.pop_back deque with
              | None -> failwith ("we did it " ^ Int.to_string a)
              | Some (deque', k) -> k a, deque')
           | Add ((a, b), k) -> k (a + b), deque
           | Let (p, k) -> p, Deque.push_back deque k
           | Parallel ((p1, p2), k) ->
             p1, Deque.push_back deque (fun i1 -> Let (p2, fun i2 -> k (i1, i2)))))
  ;;

  let to_string : t -> string =
    List.to_string ~f:(function
      | None -> "-"
      | Some (program, _) ->
        (match program with
         | Return a -> Printf.sprintf "return %d" a
         | Add ((a, b), _) -> Printf.sprintf "add %d %d" a b
         | Let _ -> "let"
         | Parallel _ -> "parallel"))
  ;;
end
