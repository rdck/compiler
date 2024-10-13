open Compiler
open Pratt
open Core
open Prelude

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

let test_case_for_expr input expect =

  let check_expr name input expect =
    let testable_expr = T.option @@ T.testable STLC.pp_expression [%equal: STLC.expression] in
    let tokens = tokenize_string input in
    let parsed = Option.map (parse_expression tokens) ~f:project_result in
    T.check testable_expr name parsed expect in

  T.test_case input `Quick (fun () ->
    check_expr input input expect
  )

let () =

  T.run "pratt parser" [

    "parse_type", [

      test_case_for_type "Z64" @@ Some STLC.Z64 ;

      test_case_for_type "Z64 -> Z64 -> Z64" @@ Some STLC.(
        Arrow (Z64, Arrow (Z64, Z64))
      ) ;

      test_case_for_type "(Z64 -> Z64) -> Z64" @@ Some STLC.(
        Arrow (Arrow (Z64, Z64), Z64)
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

      test_case_for_expr "λ x : Z64 . x" @@ Some STLC.(
        Abs ({ name = "x" ; value = Z64 }, Var "x")
      ) ;

      test_case_for_expr "λ x : Z64 . x + x" @@ Some STLC.(
        Abs ({ name = "x" ; value = Z64 }, Bin (Add, Var "x", Var "x"))
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

      test_case_for_expr "λ f : Z64 -> Z64 . λ x : Z64 . f (f x)" @@ Some STLC.(
        let body = App (Var "f", App (Var "f", Var "x")) in
        Abs (binding "f" (Arrow (Z64, Z64)), Abs (binding "x" Z64, body))
      ) ;

    ] ;
  ]
