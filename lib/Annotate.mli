(******************************************************************************)
(* STLC -> ANNOTATED *)
(******************************************************************************)

open Types

val annotate : STLC.program -> Annotated.program option
val annotate_exn : STLC.program -> Annotated.program

val forget_exn : ty Annotated.expression -> STLC.expression
