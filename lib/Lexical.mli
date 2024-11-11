type program = Lexeme.lexeme list [@@deriving equal, show]

val represent_program : program -> string
