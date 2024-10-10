open Compiler
open Token
open Parse

let test_z64 () =
  let status = match parse_type [ Z64 ] with
  | Some { result = STLC.Z64 ; remaining = [] } -> true
  | _ -> false in
  Alcotest.(check bool) "z64" status true

let test_z64_arrow_z64 () =
  let tokens = [ OpenParen ; Z64 ; Arrow ; Z64 ; ShutParen ] in
  let status = match parse_type tokens with
  | Some { result = STLC.Arrow (STLC.Z64, STLC.Z64) ; remaining = [] } -> true
  | _ -> false in
  Alcotest.(check bool) "z64 -> z64" status true

let () =
  let open Alcotest in
  run "parser" [
    "parse_type", [
      test_case "z64" `Quick test_z64 ;
      test_case "z64 -> z64" `Quick test_z64_arrow_z64 ;
    ] ;
  ]
