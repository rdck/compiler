open Compiler
open Pratt
open Core
open Prelude
open Types

(*

module T = Alcotest

let tokenize_exn s =
  ok_exn (Lex.tokenize (Lexing.from_string s))

let test_case_for_type input expect =

  let check_type name input expect =
    (* TODO: macro for pp_ty *)
    let testable_type = T.option @@ T.testable pp_ty [%equal: ty] in
    let tokens = tokenize_exn input in
    let parsed = Option.map (parse_type tokens) ~f:project_syntax in
    T.check testable_type name parsed expect in

  T.test_case input `Quick (fun () ->
    check_type input input expect
  )

let test_case_for_expr input expect =

  let check_expr name input expect =
    let testable_expr = T.option @@ T.testable STLC.pp_expression [%equal: STLC.expression] in
    let tokens = tokenize_exn input in
    let parsed = Option.map (parse_expression tokens) ~f:project_syntax in
    T.check testable_expr name parsed expect in

  T.test_case input `Quick (fun () ->
    check_expr input input expect
  )

let () =

  T.run "pratt parser" [

    "parse_type", [

      test_case_for_type "z64" @@ Some z64 ;

      test_case_for_type "z64 -> z64 -> z64" @@ Some (
        Arrow (z64, Arrow (z64, z64))
      ) ;

      test_case_for_type "(z64 -> z64) -> z64" @@ Some (
        Arrow (Arrow (z64, z64), z64)
      ) ;

    ] ;

    "parse", [

      test_case_for_expr "2 + 3" @@ Some STLC.(
        Bin (Add, Lit 2, Lit 3)
      ) ;

      test_case_for_expr "2 + 3 * 4" @@ Some STLC.(
        Bin (Add, Lit 2, Bin (Mul, Lit 3, Lit 4))
      ) ;

      test_case_for_expr "2 + 3 + 4" @@ Some STLC.(
        Bin (Add, Bin (Add, Lit 2, Lit 3), Lit 4)
      ) ;

      test_case_for_expr "2 + 3 * 4 + 5" @@ Some STLC.(
        Bin (Add, Bin (Add, Lit 2, Bin (Mul, Lit 3, Lit 4)), Lit 5)
      ) ;

      test_case_for_expr "λ x : z64 . x" @@ Some STLC.(
        Abs ({ name = "x" ; value = z64 }, Var "x")
      ) ;

      test_case_for_expr "λ x : z64 . x + x" @@ Some STLC.(
        Abs ({ name = "x" ; value = z64 }, Bin (Add, Var "x", Var "x"))
      ) ;

      test_case_for_expr "f x" @@ Some STLC.(
        App (Var "f", Var "x")
      ) ;

      test_case_for_expr "f x y" @@ Some STLC.(
        App (App (Var "f", Var "x"), Var "y")
      ) ;

      test_case_for_expr "a * f x" @@ Some STLC.(
        Bin (Mul, Var "a", App (Var "f", Var "x"))
      ) ;

      test_case_for_expr "λ f : z64 -> z64 . λ x : z64 . f (f x)" @@ Some STLC.(
        let body = App (Var "f", App (Var "f", Var "x")) in
        Abs (binding "f" (Arrow (z64, z64)), Abs (binding "x" z64, body))
      ) ;

    ] ;
  ]

*)
