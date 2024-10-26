(******************************************************************************)
(* STLC -> ANNOTATED *)
(******************************************************************************)

open Types

val annotate_program : STLC.program -> (Annotated.program, string) result
