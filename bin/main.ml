open Kanni.Simulate

let () =
  let module Processor = Processor (Source) in
  Processor.eval_program
    Instruction.(
      ( [ Store (Literal 42)
        ; Store (Literal 1)
        ; Const 0
        ; Load
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ]
      , 0 ))
    []
    0
  |> fst
  |> RegisterArray.show
  |> print_endline
;;
