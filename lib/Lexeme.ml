open Core
include LexemeData

let represent_lexeme = function
  | Identifier id -> id
  | Constructor id -> id
  | Literal l -> sprintf "%d" l
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Period -> "."
  | Colon -> ":"
  | Arrow -> "->"
  | Lambda -> "λ"
  | OpenParen -> "("
  | ShutParen -> ")"
  | Equal -> "="
  | Bar -> "|"
  | Let -> "let"
  | Recursive -> "recursive"
  | In -> "in"
  | Type -> "type"
  | Of -> "of"
  | Match -> "match"
  | With -> "with"
  | End -> "end"
  | EOF -> "EOF"
