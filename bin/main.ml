open! Core
open Kanni.Parallel

module Parallel = Parallel (struct
  type a = int

  let to_string = Int.to_string
end)

let add (a, b) = Parallel.return (a + b)

let example =
  Parallel.(
    par
      ( (par ((return 1, return 2), add), add (3, 4))
      , fun (x, y) -> par ((add (x, y), add (y, 5)), add) ))
;;

let rec example2 =
  Parallel.(
    function
    | 0 -> return 1
    | n -> par ((example2 (n - 1), example2 (n - 1)), add))
;;

let run ?(p = 6) example =
  let fixlength s =
    Printf.sprintf "|%10s " (String.sub ~pos:0 ~len:(Int.min 10 (String.length s)) s)
  in
  let trace, a = Parallel.out ~p example in
  List.iteri
    ~f:(fun i procs ->
      Printf.printf "[%2d]  " i;
      List.iter ~f:(fixlength >>> print_string) procs;
      Printf.printf "\n")
    trace;
  Printf.printf ">>> %d\n" a
;;

let () = run ~p:10 (example2 5)
