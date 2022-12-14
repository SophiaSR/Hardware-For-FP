open! Core
open Kanni.Parallel

module Test (P : PARALLEL) : sig
  val run : ?title:string -> ?p:int -> P.t -> unit
end = struct
  (* let run ?(p = 6) example =
    let trace, a = P.run ~p example in
    List.iteri
      ~f:(fun i procs ->
        Printf.printf "%2d:  " i;
        List.iter
          ~f:
            ((function
              | None -> "[-] "
              | Some (_, i) -> Printf.sprintf "[%1d] " i)
            >>> print_string)
          procs;
        Printf.printf "\n")
      trace;
    Printf.printf ">>> %s\n" (P.A.to_string a)
  ;; *)

  let run ?(title = "") ?(p = 6) example =
    let trace, a = P.run ~p example in
    let utilizations, working_sets =
      trace
      |> List.map ~f:(fun procs ->
           let utilization = List.count ~f:Option.is_some procs in
           let working_set =
             List.sum
               (module Int)
               ~f:
                 (function
                  | None -> 0
                  | Some (_, n) -> n + 1)
               procs
           in
           utilization, working_set)
      |> List.unzip
    in
    let show_list l =
      let scaled = List.map ~f:(fun n -> Float.of_int n /. Float.of_int p) l in
      Printf.sprintf
        "[%s]"
        (List.fold
           ~f:( ^ )
           ~init:""
           (List.map ~f:(Float.to_string >>> Printf.sprintf "%s,") scaled))
    in
    Printf.printf "result = %s\n" (P.A.to_string a);
    Printf.printf "title = \"Benchmark \\\"%s\\\" ($p$ = %d)\"\n" title p;
    Printf.printf "utilizations = %s\n" (show_list utilizations);
    Printf.printf "working_sets = %s\n" (show_list working_sets);
    Printf.printf "\n";
    Out_channel.flush Out_channel.stdout;
    ignore (In_channel.input_line In_channel.stdin)
  ;;
end

let () =
  let module P = Parallel (Int) in
  let add (a, b) = P.return (a + b) in
  let example1 =
    P.(
      par
        ( (par ((return 1, return 2), add), add (3, 4))
        , fun (x, y) -> par ((add (x, y), add (y, 5)), add) ))
  in
  let rec example2 =
    P.(
      function
      | 0 -> return 1
      | n -> par ((example2 (n - 1), example2 (n - 1)), add))
  in
  let module Test = Test (P) in
  Test.run ~title:"Feeling 22" ~p:2 example1;
  Test.run ~title:"Slow Exponentiation" ~p:2 (example2 5);
  Test.run ~title:"Slow Exponentiation" ~p:10 (example2 5)
;;

module IntList = struct
  type t = int list

  let to_string = List.to_string ~f:Int.to_string
end

let () =
  let module P = Parallel (IntList) in
  let rec merge = function
    | [], l2 -> l2
    | l1, [] -> l1
    | x :: xs, y :: ys ->
      if x < y then x :: merge (xs, y :: ys) else y :: merge (x :: xs, ys)
  in
  let is_sorted = List.is_sorted ~compare:Int.compare in
  let rec msort =
    P.(
      fun l ->
        if is_sorted l
        then return l
        else (
          let n = List.length l / 2 in
          let l1, l2 = List.take l n, List.drop l n in
          par ((msort l1, msort l2), merge >>> return)))
  in
  (* let rec msort_askew =
    P.(
      fun l ->
        if is_sorted l
        then return l
        else (
          let n = 2 * List.length l / 3 in
          let l1, l2 = List.take l n, List.drop l n in
          par ((msort_askew l1, msort_askew l2), merge >>> return)))
  in *)
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
  let module Test = Test (P) in
  let msort2 l = P.bind (msort l, fun l' -> P.bind (msort (List.rev l'), P.return)) in
  (* let msort_askew2 l =
    P.bind (msort_askew l, fun l' -> P.bind (msort_askew (List.rev l'), P.return))
  in *)
  Test.run ~title:"Merge Sort" ~p:2 (msort example_list);
  Test.run ~title:"Merge Sort" ~p:8 (msort example_list);
  Test.run ~title:"Merge Sort" ~p:20 (msort example_list);
  Test.run ~title:"Merge Sort, Twice" ~p:8 (msort2 example_list);
  Test.run ~title:"Merge Sort, Twice" ~p:20 (msort2 example_list)
;;

let () =
  let module Set = Set.Make (Int) in
  let module P =
    Parallel (struct
      type t = Set.t

      let to_string = Set.to_list >>> List.to_string ~f:Int.to_string
    end)
  in
  let bfs graph s =
    let rec unions = function
      | [] -> P.return Set.empty
      | [ s ] -> P.return s
      | l ->
        let n = List.length l / 2 in
        let l1, l2 = List.take l n, List.drop l n in
        P.(par ((unions l1, unions l2), fun (s1, s2) -> return (Set.union s1 s2)))
    in
    let rec loop seen frontier =
      if Set.is_empty frontier
      then P.return seen
      else (
        let seen = Set.union seen frontier in
        let frontiers = List.filteri ~f:(fun i _ -> Set.mem frontier i) graph in
        P.bind
          ( unions frontiers
          , fun ngf ->
              let frontier = Set.diff ngf seen in
              loop seen frontier ))
    in
    loop Set.empty (Set.singleton s)
  in
  let module Test = Test (P) in
  let example =
    [ Set.of_list [ 24; 48; 33; 59; 44; 95 ]
    ; Set.of_list [ 24; 56; 75; 84 ]
    ; Set.of_list [ 46; 32; 76; 34; 50; 21; 54; 24; 93 ]
    ; Set.of_list [ 7; 94; 23 ]
    ; Set.of_list [ 80; 47 ]
    ; Set.of_list [ 26; 88; 35; 72; 68; 16; 92 ]
    ; Set.of_list [ 84 ]
    ; Set.of_list [ 47; 39; 73; 95; 31 ]
    ; Set.of_list [ 65; 3; 85; 54; 84 ]
    ; Set.of_list [ 88; 93; 27; 50; 80; 44; 51; 34; 64; 76 ]
    ; Set.of_list [ 30; 3; 69; 19; 66; 53; 71; 14; 32 ]
    ; Set.of_list [ 7; 23; 2; 0; 84; 17; 59; 67; 18; 72 ]
    ; Set.of_list [ 45; 7 ]
    ; Set.of_list [ 25; 63; 90; 39; 77 ]
    ; Set.of_list [ 96; 78 ]
    ; Set.of_list [ 87; 88; 82; 46; 19; 80; 99; 13 ]
    ; Set.of_list [ 86; 36 ]
    ; Set.of_list [ 27 ]
    ; Set.of_list [ 20; 38; 64; 65 ]
    ; Set.of_list [ 29; 61; 38; 39; 57; 64 ]
    ; Set.of_list [ 85; 2; 97; 3 ]
    ; Set.of_list [ 39; 29; 81 ]
    ; Set.of_list [ 28; 89; 52; 43; 77; 40 ]
    ; Set.of_list [ 19; 92; 67; 58 ]
    ; Set.of_list [ 71; 27; 85; 76; 5 ]
    ; Set.of_list [ 4; 41; 20 ]
    ; Set.of_list [ 0; 41 ]
    ; Set.of_list [ 39; 37; 16; 42; 48; 89; 79 ]
    ; Set.of_list [ 44; 78; 23 ]
    ; Set.of_list [ 50 ]
    ; Set.of_list [ 9; 6; 40; 46; 42; 87; 3; 60; 61; 25 ]
    ; Set.of_list [ 43; 78; 32; 41 ]
    ; Set.of_list [ 29 ]
    ; Set.of_list [ 7; 90 ]
    ; Set.of_list [ 71; 93 ]
    ; Set.of_list [ 2; 9; 91; 77 ]
    ; Set.of_list [ 72; 37; 47; 46; 58; 43; 36; 79; 14 ]
    ; Set.of_list [ 23; 1; 12; 73 ]
    ; Set.of_list [ 59; 4; 13 ]
    ; Set.of_list [ 86; 73 ]
    ; Set.of_list [ 95; 43; 72; 64; 63; 89; 91; 49 ]
    ; Set.of_list [ 71; 91; 6; 15 ]
    ; Set.of_list [ 7; 0; 79; 40; 31; 81; 18; 47 ]
    ; Set.of_list [ 71; 91; 47; 46; 41; 20; 79 ]
    ; Set.of_list [ 39; 43 ]
    ; Set.of_list [ 70; 52; 57; 20; 90; 66; 34; 91 ]
    ; Set.of_list [ 56; 44; 42; 86; 90 ]
    ; Set.of_list [ 3 ]
    ; Set.of_list [ 66; 37; 89; 9 ]
    ; Set.of_list [ 82; 64; 83; 75; 78; 44; 85; 95; 18; 93 ]
    ; Set.of_list [ 37; 51; 70; 53; 68; 63; 17 ]
    ; Set.of_list [ 52; 31; 70; 61; 7; 10; 66; 62; 63; 57 ]
    ; Set.of_list [ 86; 57; 92; 63 ]
    ; Set.of_list [ 7; 81; 12; 24; 35; 30; 50 ]
    ; Set.of_list [ 23; 91; 89; 75; 54; 22; 76 ]
    ; Set.of_list [ 38; 22 ]
    ; Set.of_list [ 11; 53; 56; 58; 51; 32; 57; 49; 94; 86 ]
    ; Set.of_list [ 81; 38; 41; 11; 39; 23; 68; 56; 18; 12 ]
    ; Set.of_list [ 40; 1; 37; 20; 85; 82; 30; 23; 35 ]
    ; Set.of_list [ 8; 2; 14; 49; 90; 37; 70; 74; 48; 22 ]
    ; Set.of_list [ 92; 83; 14; 1; 40; 76; 8; 42; 47; 58 ]
    ; Set.of_list [ 73; 65; 90; 44; 18; 16 ]
    ; Set.of_list [ 60; 18; 39; 35; 46; 25; 48 ]
    ; Set.of_list [ 93; 25 ]
    ; Set.of_list [ 14; 37; 81; 36; 63; 30; 4; 38; 71; 66 ]
    ; Set.of_list [ 90; 36; 8; 22; 25 ]
    ; Set.of_list [ 21; 54; 53; 42; 66; 22; 51; 31; 19; 28 ]
    ; Set.of_list [ 15; 85; 53; 8; 6; 21; 95 ]
    ; Set.of_list [ 3; 87; 26; 31 ]
    ; Set.of_list [ 54; 32; 82; 66; 6; 43; 65; 24 ]
    ; Set.of_list [ 94; 35; 47; 11; 96; 19 ]
    ; Set.of_list [ 3; 53 ]
    ; Set.of_list [ 68; 98; 87; 43 ]
    ; Set.of_list [ 52; 42; 86; 64; 98; 3; 39; 84 ]
    ; Set.of_list [ 72; 18; 95; 74; 62; 35; 7; 48; 52; 11 ]
    ; Set.of_list [ 47; 15; 55; 90; 25; 51; 68; 79; 12; 64 ]
    ; Set.of_list [ 96; 5; 65; 81; 88; 48; 50; 43 ]
    ; Set.of_list [ 41; 91; 87; 82; 34; 60; 73; 35 ]
    ; Set.of_list [ 34 ]
    ; Set.of_list [ 50; 0; 18; 17; 58; 35; 4; 98; 39 ]
    ; Set.of_list [ 60; 13; 92; 4; 27 ]
    ; Set.of_list [ 78; 55; 9; 70; 37; 10; 16; 30 ]
    ; Set.of_list [ 10; 31; 41; 30; 42; 4; 83 ]
    ; Set.of_list [ 13; 11; 73; 20 ]
    ; Set.of_list [ 12; 96; 83; 61; 54; 31; 78; 59 ]
    ; Set.of_list [ 30; 27 ]
    ; Set.of_list [ 89; 55; 75 ]
    ; Set.of_list [ 31; 11 ]
    ; Set.of_list [ 22; 77; 92; 71; 21 ]
    ; Set.of_list [ 25 ]
    ; Set.of_list [ 72; 66; 55; 35; 77; 43; 46; 8 ]
    ; Set.of_list [ 49; 82; 94; 0; 51; 75 ]
    ; Set.of_list [ 41; 90; 19; 74; 55 ]
    ; Set.of_list [ 4; 20; 12 ]
    ; Set.of_list [ 36; 29; 27; 97; 42; 93; 18; 21 ]
    ; Set.of_list [ 53; 24; 23 ]
    ; Set.of_list [ 36; 14; 72; 20; 24; 73 ]
    ; Set.of_list [ 55; 58; 72; 9; 68; 76 ]
    ; Set.of_list [ 4; 2; 53; 81; 59; 74; 56; 79; 64; 42 ]
    ; Set.of_list [ 32; 62; 77; 97; 33; 65; 23; 69; 61 ]
    ]
  in
  Test.run ~title:"Parallel BFS" ~p:8 (bfs example 0);
  Test.run ~title:"Parallel BFS" ~p:16 (bfs example 0);
  Test.run ~title:"Parallel BFS" ~p:32 (bfs example 0)
;;
