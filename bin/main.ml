open! Core
open Kanni.Parallel

let () =
  let module A = struct
    type a = int

    let to_string = Int.to_string
  end
  in
  let module Parallel = Parallel (A) in
  let add (a, b) = Parallel.return (a + b) in
  let example1 =
    Parallel.(
      par
        ( (par ((return 1, return 2), add), add (3, 4))
        , fun (x, y) -> par ((add (x, y), add (y, 5)), add) ))
  in
  let rec example2 =
    Parallel.(
      function
      | 0 -> return 1
      | n -> par ((example2 (n - 1), example2 (n - 1)), add))
  in
  let run ?(p = 6) example =
    let trace, a = Parallel.run ~p example in
    List.iteri
      ~f:(fun i procs ->
        Printf.printf "[%2d]  " i;
        List.iter
          ~f:
            ((function
              | None -> "[--] "
              | Some (_, i) -> Printf.sprintf "[%2d] " i)
            >>> print_string)
          procs;
        Printf.printf "\n")
      trace;
    Printf.printf ">>> %s\n" (A.to_string a)
  in
  run ~p:2 example1;
  run ~p:2 (example2 5);
  run ~p:10 (example2 5)
;;

let () =
  let module A = struct
    type a = int list

    let to_string = List.to_string ~f:Int.to_string
  end
  in
  let module Parallel = Parallel (A) in
  let rec merge = function
    | [], l2 -> l2
    | l1, [] -> l1
    | x :: xs, y :: ys ->
      if x < y then x :: merge (xs, y :: ys) else y :: merge (x :: xs, ys)
  in
  let rec msort =
    Parallel.(
      function
      | [] -> return []
      | [ x ] -> return [ x ]
      | l ->
        let n = List.length l / 2 in
        let l1, l2 = List.take l n, List.drop l n in
        par ((msort l1, msort l2), merge >>> return))
  in
  let example_list =
    [ 78
    ; 29
    ; 43
    ; 59
    ; 48
    ; 49
    ; 84
    ; 45
    ; 66
    ; 65
    ; 97
    ; 11
    ; 27
    ; 55
    ; 19
    ; 26
    ; 75
    ; 41
    ; 37
    ; 13
    ; 82
    ; 31
    ; 46
    ; 99
    ; 86
    ; 4
    ; 53
    ; 7
    ; 72
    ; 83
    ; 20
    ; 22
    ; 98
    ; 62
    ; 64
    ; 85
    ; 73
    ; 79
    ; 21
    ; 5
    ; 17
    ; 56
    ; 90
    ; 8
    ; 3
    ; 6
    ; 95
    ; 35
    ; 74
    ; 2
    ; 10
    ; 30
    ; 9
    ; 33
    ; 63
    ; 12
    ; 92
    ; 87
    ; 57
    ; 76
    ; 70
    ; 24
    ; 47
    ; 51
    ; 54
    ; 18
    ; 36
    ; 23
    ; 61
    ; 40
    ; 77
    ; 44
    ; 16
    ; 91
    ; 60
    ; 52
    ; 15
    ; 67
    ; 68
    ; 25
    ; 69
    ; 50
    ; 1
    ; 81
    ; 94
    ; 71
    ; 42
    ; 34
    ; 88
    ; 96
    ; 39
    ; 80
    ; 14
    ; 28
    ; 32
    ; 38
    ; 58
    ; 93
    ; 89
    ; 100
    ]
  in
  let run ?(p = 6) example =
    let trace, a = Parallel.run ~p example in
    List.iteri
      ~f:(fun i procs ->
        Printf.printf "[%2d]  " i;
        List.iter
          ~f:
            ((function
              | None -> "[--] "
              | Some (_, i) -> Printf.sprintf "[%2d] " i)
            >>> print_string)
          procs;
        Printf.printf "\n")
      trace;
    Printf.printf ">>> %s\n" (A.to_string a)
  in
  run ~p:8 (msort example_list)
;;
