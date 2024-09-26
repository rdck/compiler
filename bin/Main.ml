open Core
open Compiler

let _ =
  let compiled = Example.three
    |> Annotate.annotate_exn
    |> Lift.lift
    |> Translate.compile_program
    |> CmmBackend.compile_program in
  printf "%s\n" (Cmm.represent compiled)
