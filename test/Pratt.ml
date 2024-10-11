open Compiler
open Token
open Pratt
open Core

let testable_expr = Alcotest.testable STLC.pp_expression [%equal: STLC.expression]
let testable_expr_opt = Alcotest.option testable_expr
let verify = Alcotest.check testable_expr_opt

let () =

  let open Alcotest in
  run "pratt parser" [ "parse", [

    test_case "2 + 3" `Quick (fun () ->
      let tokens = [ Literal 2 ; Plus ; Literal 3 ] in
      let parsed = Option.map (parse tokens) ~f:(fun p -> p.result) in
      let expect = STLC.(Bin (Add, Lit 2, Lit 3)) in
      verify "2 + 3" parsed (Some expect)
    ) ;

    test_case "2 + 3 * 4" `Quick (fun () ->
      let tokens = [ Literal 2 ; Plus ; Literal 3 ; Star ; Literal 4 ] in
      let parsed = Option.map (parse tokens) ~f:(fun p -> p.result) in
      let expect = STLC.(Bin (Add, Lit 2, Bin (Mul, Lit 3, Lit 4))) in
      verify "2 + 3 * 4" parsed (Some expect)
    ) ;

    test_case "2 + 3 + 4" `Quick (fun () ->
      let tokens = [ Literal 2 ; Plus ; Literal 3 ; Plus ; Literal 4 ; ] in
      let parsed = Option.map (parse tokens) ~f:(fun p -> p.result) in
      let expect = STLC.(Bin (Add, Bin (Add, Lit 2, Lit 3), Lit 4)) in
      verify "2 + 3 + 4" parsed (Some expect)
    ) ;

    test_case "2 + 3 * 4 + 5" `Quick (fun () ->
      let tokens = [
        Literal 2 ;
        Plus      ;
        Literal 3 ;
        Star      ;
        Literal 4 ;
        Plus      ;
        Literal 5 ;
      ] in
      let parsed = Option.map (parse tokens) ~f:(fun p -> p.result) in
      let expect = STLC.(Bin (Add, Bin (Add, Lit 2, Bin (Mul, Lit 3, Lit 4)), Lit 5)) in
      verify "2 + 3 * 4 * 5" parsed (Some expect)

      ) ;
    ] ; ]
