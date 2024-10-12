open Compiler
open RecursiveDescent
open Core

module T = Alcotest

let project_result p = p.result

let tokenize_string =
  Fn.compose Lex.tokenize Lexing.from_string

let test_case_for_type input expect =

  let check_type name input expect =
    let testable_type = T.option @@ T.testable STLC.pp_ty [%equal: STLC.ty] in
    let tokens = tokenize_string input in
    let parsed = Option.map (parse_type tokens) ~f:project_result in
    T.check testable_type name parsed expect in

  T.test_case input `Quick (fun () ->
    check_type input input expect
  )

let () =

  T.run "pure recursive descent parser" [

    "parse_type", [

      test_case_for_type "Z64" @@ Some STLC.Z64 ;
      test_case_for_type "(Z64 -> Z64)" @@ Some STLC.(Arrow (Z64, Z64)) ;

    ] ;

  ]
