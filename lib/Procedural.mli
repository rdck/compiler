(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

include module type of ProceduralData

val represent_ty              : ty              -> string
val represent_binop           : binop           -> string
val represent_assignable      : assignable      -> string
val represent_expression      : expression      -> string
val represent_statement       : statement       -> string
val represent_program         : program         -> string
