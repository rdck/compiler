open Core

type program = Lexeme.lexeme list [@@deriving equal, show]

let represent_program program =
  String.concat ~sep:" " (List.map program ~f:Lexeme.represent_lexeme)
