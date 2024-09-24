open Core
open Compiler

let _ =
  let compiled = Example.three
    |> Annotate.annotate_exn
    |> Lift.lift
    |> Translate.compile_program in
  printf "%s\n" ([%show: TAC.program] compiled)
