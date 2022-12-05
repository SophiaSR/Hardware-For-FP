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
    program_with_parent option
    * program_with_parent Deque.t
    * (parent * continuation) Map.t

  type t = processor list

  let start (program : program) : t =
    List.init
      ~f:(fun i ->
        ( (match i with
          | 0 -> Some (None, program)
          | _ -> None)
        , Deque.create ()
        , Map.create () ))
      p
  ;;

  let step : t -> t =
   fun processors ->
    let processors_conts = List.map ~f:(fun (_, _, conts) -> conts) processors in
    List.mapi
      ~f:(fun i (opt, deque, conts) ->
        ( Option.bind
            ~f:(fun (parent, program) ->
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
                    Some (parent', k a)
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
                    Deque.dequeue_back deque))
              | Add ((a, b), k) -> Some (parent, k (a + b))
              | Parallel ((p1, p2), k) ->
                let key = Unique_id.create () in
                Map.add_exn ~key ~data:(parent, Wait2 k) conts;
                Deque.enqueue_back deque (Some (i, key, `Right), p2);
                Some (Some (i, key, `Left), p1))
            opt
        , deque
        , conts ))
      processors
 ;;

  let to_string : t -> string =
    List.to_string ~f:(function
        | None, _, _ -> "-"
        | Some (_, program), _, _ ->
          (match program with
          | Return a -> Printf.sprintf "return %d" a
          | Add ((a, b), _) -> Printf.sprintf "add %d %d" a b
          | Parallel _ -> "parallel"))
  ;;
end
