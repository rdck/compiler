(******************************************************************************)
(* LIFTED LAMBDA CALCULUS *)
(******************************************************************************)

open Symbol
include module type of ApexData

(* curried term constructor *)
val annotate : 'a node -> 'a -> 'a expression

(* curried closure constructor *)
val closure : symbol -> 'a expression list -> 'a closure

(* term representations *)
val represent_binop : binop -> string
val represent_pattern : pattern -> string
val represent_term : term -> string
val represent_definition : definition -> string
val represent_program : program -> string
