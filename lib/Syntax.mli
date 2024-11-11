(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS *)
(******************************************************************************)

include module type of SyntaxData

val represent_binop : binop -> string
val represent_pattern : pattern -> string
val represent_expression : expression -> string
val represent_program : program -> string
