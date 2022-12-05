open! Core

type program =
  | Return of int
  | Add of (int * int) * (int -> program)
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

module Unique_id = Unique_id.Int ()

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
  module Map = Hashtbl.Make (Unique_id)

  type parent = (int * Unique_id.t * [ `Left | `Right ]) option
  type program_with_parent = parent * program

  type continuation =
    | Wait1 of (int -> program)
    | Wait2 of (int * int -> program)

  type processor =
    (program_with_parent * program_with_parent Deque.t) option
    * (parent * continuation) Map.t

  type t = processor list

  let start (program : program) : t =
    List.init
      ~f:(function
        | 0 -> Some ((None, program), Deque.empty), Map.create ()
        | _ -> None, Map.create ())
      p
  ;;

  let step : t -> t =
   fun processors ->
    let processors_conts = List.map ~f:snd processors in
    List.mapi
      ~f:(fun i (opt, conts) ->
        ( Option.bind
            ~f:(fun ((parent, program), deque) ->
              match program with
              | Return a ->
                (match parent with
                | None ->
                  Printf.printf "stretchy bird says: %d\n" a;
                  None
                | Some (processor, key, side) ->
                  let cont_table = List.nth_exn processors_conts processor in
                  let parent', cont = Map.find_exn cont_table key in
                  (match cont with
                  | Wait1 k ->
                    Map.remove cont_table key;
                    Some ((parent', k a), deque)
                  | Wait2 k ->
                    Map.remove cont_table key;
                    Map.add_exn
                      cont_table
                      ~key
                      ~data:
                        ( parent'
                        , Wait1
                            (match side with
                            | `Left -> fun b -> k (a, b)
                            | `Right -> fun b -> k (b, a)) );
                    Option.map (Deque.pop_back deque) ~f:(fun (deque', program') ->
                        program', deque')))
              | Add ((a, b), k) -> Some ((parent, k (a + b)), deque)
              | Parallel ((p1, p2), k) ->
                let key = Unique_id.create () in
                Map.add_exn ~key ~data:(parent, Wait2 k) conts;
                Some
                  ( (Some (i, key, `Left), p1)
                  , Deque.push_back deque (Some (i, key, `Right), p2) ))
            opt
        , conts ))
      processors
 ;;

  let to_string : t -> string =
    List.to_string ~f:(function
        | None, _ -> "-"
        | Some ((_, program), _), _ ->
          (match program with
          | Return a -> Printf.sprintf "return %d" a
          | Add ((a, b), _) -> Printf.sprintf "add %d %d" a b
          | Parallel _ -> "parallel"))
  ;;
end
