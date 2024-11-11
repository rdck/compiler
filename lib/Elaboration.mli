(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

include module type of ElaborationData

(* curried expression constructor *)
val annotate : 'a node -> 'a -> 'a expression

val represent_binop : binop -> string
val represent_pattern : pattern -> string
val represent_term : term -> string
val represent_program : program -> string
