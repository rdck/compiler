(******************************************************************************)
(* STLC -> Annotated *)
(******************************************************************************)

open Annotated

val annotate : STLC.expression -> ty expression option
val annotate_exn : STLC.expression -> ty expression

val forget_exn : ty expression -> STLC.expression
