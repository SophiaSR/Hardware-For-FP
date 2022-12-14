open! Core
module Unique_id = Unique_id.Int ()

let ( >>> ) f g x = g (f x)

let ( <|> ) opt f =
  match opt with
  | None -> f ()
  | Some x -> Some x
;;

module type SHOW = sig
  type t

  val to_string : t -> string
end

module type PARALLEL = sig
  module A : SHOW

  type t

  (* commutative monad *)
  val return : A.t -> t
  val map : (A.t -> A.t) -> t -> t
  val bind : t * (A.t -> t) -> t
  val par : (t * t) * (A.t * A.t -> t) -> t

  (* machine *)
  val run : p:int -> t -> (string * int) option list list * A.t
end

module Parallel (A : SHOW) : PARALLEL with module A = A = struct
  module A = A

  type a = A.t

  type t =
    | Return of a
    | Let of t * (a -> t)
    | Parallel of (t * t) * (a * a -> t)

  let rec to_string = function
    | Return a -> Printf.sprintf "return[%s]" (A.to_string a)
    | Let (e, _) -> Printf.sprintf "let[%s]" (to_string e)
    | Parallel ((e1, e2), _) ->
      Printf.sprintf "parallel[%s][%s]" (to_string e1) (to_string e2)
  ;;

  let return (a : a) : t = Return a
  let map (f : a -> a) (x : t) = Let (x, fun a -> Return (f a))
  let bind ((t : t), (f : a -> t)) : t = Let (t, f)
  let par ((t1, t2), k) : t = Parallel ((t1, t2), k)

  module Map = Hashtbl.Make (Unique_id)

  type parent = (int * Unique_id.t * [ `Left | `Right ]) option
  type program_with_parent = parent * t

  type continuation =
    | Wait1 of (a -> t)
    | Wait2 of (a * a -> t)

  type processor =
    program_with_parent option
    * program_with_parent Deque.t
    * (parent * continuation) Map.t

  type processors = processor list

  let run ~p (graph : t) : (string * int) option list (* each is length p *) list * a =
    let start : processors =
      List.init
        ~f:(fun i ->
          ( (match i with
             | 0 -> Some (None, graph)
             | _ -> None)
          , Deque.create ()
          , Map.create () ))
        p
    in
    let final : a option ref = ref None in
    let steal : processors -> program_with_parent option =
     fun processors ->
      let processor_deques = List.map ~f:(fun (_, deque, _) -> deque) processors in
      let rec aux (deques : 'a Deque.t list) : 'a option =
        match deques with
        | [] -> None
        | deque :: deques ->
          (match Deque.dequeue_front deque with
           | None -> aux deques
           | Some a -> Some a)
      in
      aux processor_deques
    in
    let step : processors -> processors =
     fun processors ->
      let processors_conts = List.map ~f:(fun (_, _, conts) -> conts) processors in
      processors
      |> List.mapi ~f:(fun i (opt, deque, conts) ->
           ( Option.bind
               ~f:(fun (parent, program) ->
                 match program with
                 | Return a ->
                   (match parent with
                    | None ->
                      final := Some a;
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
                 | Let (p, k) ->
                   let key = Unique_id.create () in
                   Map.add_exn ~key ~data:(parent, Wait1 k) conts;
                   Some (Some (i, key, `Left), p)
                 | Parallel ((p1, p2), k) ->
                   let key = Unique_id.create () in
                   Map.add_exn ~key ~data:(parent, Wait2 k) conts;
                   Deque.enqueue_back deque (Some (i, key, `Right), p2);
                   Some (Some (i, key, `Left), p1))
               opt
           , deque
           , conts ))
      |> List.map ~f:(fun (opt, deque, conts) ->
           (opt <|> fun () -> steal processors), deque, conts)
    in
    let rec loop (processors : processors) : (string * int) option list list * a =
      match !final with
      | Some a -> [], a
      | None ->
        let state =
          List.map
            ~f:(fun (opt, deque, _) ->
              Option.map ~f:(fun (_, p) -> to_string p, Deque.length deque) opt)
            processors
        in
        let states, a = loop (step processors) in
        state :: states, a
    in
    loop start
  ;;
end
