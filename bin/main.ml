open Kanni.Simulate

let () =
  let module Processor = Processor (Source) in
  Processor.eval_program
    Instruction.(
      ( [ Const 0
        ; Const 1
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ; Add (Register 0, Register 1)
        ]
      , 0 ))
    []
  |> RegisterArray.show
  |> print_endline
;;
