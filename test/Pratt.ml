open Compiler
open Pratt
open Core
open Symbol
open Types
open Result.Let_syntax
module T = Alcotest

let tokenize_exn s = Result.ok_or_failwith (Lexer.tokenize (Lexing.from_string s))

let test_case_for_type input expect =
  let check_type name input expect =
    let testable_type = T.testable pp_ty [%equal: ty] in
    let testable_result = T.result testable_type T.string in
    let tokens = tokenize_exn input in
    let parsed = Result.map (parse_type tokens) ~f:project_syntax in
    T.check testable_result name parsed expect
  in
  T.test_case input `Quick (fun () -> check_type input input expect)


let test_case_for_expr input expect =
  let check_expr name input expect =
    let testable_expr = T.testable Syntax.pp_expression [%equal: Syntax.expression] in
    let testable_result = T.result testable_expr T.string in
    let tokens = tokenize_exn input in
    let parsed = Result.map (parse_expression tokens) ~f:project_syntax in
    T.check testable_result name parsed expect
  in
  T.test_case input `Quick (fun () -> check_expr input input expect)


let ilit i = Syntax.Lit (IntegerLiteral i)
let blit b = Syntax.Lit (BooleanLiteral b)

let () =
  T.run
    "pratt parser"
    [ ( "parse_type"
      , [ test_case_for_type "z64" @@ return z64
        ; test_case_for_type "z64 -> z64 -> z64" @@ return (Arrow (z64, Arrow (z64, z64)))
        ; test_case_for_type "(z64 -> z64) -> z64"
          @@ return (Arrow (Arrow (z64, z64), z64))
        ] )
    ; ( "parse"
      , [ test_case_for_expr "2 + 3" @@ return Syntax.(Bin (Add, ilit 2, ilit 3))
        ; test_case_for_expr "2 + 3 * 4"
          @@ return Syntax.(Bin (Add, ilit 2, Bin (Mul, ilit 3, ilit 4)))
        ; test_case_for_expr "2 + 3 + 4"
          @@ return Syntax.(Bin (Add, Bin (Add, ilit 2, ilit 3), ilit 4))
        ; test_case_for_expr "2 + 3 * 4 + 5"
          @@ return
               Syntax.(Bin (Add, Bin (Add, ilit 2, Bin (Mul, ilit 3, ilit 4)), ilit 5))
        ; test_case_for_expr "λ x : z64 . x"
          @@ return Syntax.(Abs ({ name = "x"; value = z64 }, Var "x"))
        ; test_case_for_expr "λ x : z64 . x + x"
          @@ return
               Syntax.(Abs ({ name = "x"; value = z64 }, Bin (Add, Var "x", Var "x")))
        ; test_case_for_expr "f x" @@ return Syntax.(App (Var "f", Var "x"))
        ; test_case_for_expr "f x y"
          @@ return Syntax.(App (App (Var "f", Var "x"), Var "y"))
        ; test_case_for_expr "a * f x"
          @@ return Syntax.(Bin (Mul, Var "a", App (Var "f", Var "x")))
        ; test_case_for_expr "λ f : z64 -> z64 . λ x : z64 . f (f x)"
          @@ return
               Syntax.(
                 let body = App (Var "f", App (Var "f", Var "x")) in
                 Abs (binding "f" (Arrow (z64, z64)), Abs (binding "x" z64, body)))
        ] )
    ]
