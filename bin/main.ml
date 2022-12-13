open! Core
open Kanni.Scheduler

let example =
  Program.(
    Parallel
      ( (Parallel ((Return 1, Return 2), add), add (3, 4))
      , fun (x, y) -> Parallel ((add (x, y), add (y, 5)), add) ))

and example2 = Program.(While (10, fun i -> Add ((i, -1), fun a -> Return a)))

and example3 =
  Program.(
    While
      ( 10
      , fun i ->
          Parallel ((return i, return (-i)), fun _ -> Add ((i, -1), fun a -> Return a)) ))

and example4 =
  Program.(
    While
      ( 10
      , fun i ->
          Parallel
            ( ( While (2 * i, fun j -> Add ((j, -1), return))
              , While (i, fun j -> Add ((j, -1), return)) )
            , fun _ -> Add ((i, -1), return) ) ))
;;

let rec example5 =
  Program.(
    function
    | 0 -> return 0
    | n -> Parallel ((example5 (n - 1), example5 (n - 1)), add))
;;

let rec example5 =
  Program.(
    function
    | 0 -> return 0
    | n -> Parallel ((example5 (n - 1), example5 (n - 1)), fun (x, y) -> return (x + y)))
;;

let () =
  let module Scheduler =
    Scheduler (struct
      let p = 6
    end)
  in
  ignore
    (Fn.apply_n_times
       ~n:100
       (fun s ->
         let () = Printf.printf "%s\n" (Scheduler.to_string s) in
         Scheduler.step s)
       (Scheduler.start (example5 5)))
;;
