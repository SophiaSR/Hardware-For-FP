type program =
  | Return of int
  | Add of int * int * (int -> program)
  | Parallel of program * program * (int * int -> program)

let return x = Return x
let add (x, y) = Add (x, y, return)

let example =
  Add
    (3, 4, fun x -> Parallel (Add (1, 2, fun ab -> add (ab, x)), Add (x, 5, return), add))
;;
