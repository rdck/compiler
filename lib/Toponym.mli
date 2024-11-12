include module type of ToponymData

(* curried expression constructor *)
val annotate : 'a node -> 'a -> 'a expression
val represent_binop : binop -> string
val represent_pattern : pattern -> string
val represent_program : program -> string
