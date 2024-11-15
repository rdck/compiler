open Core

type lexeme =
  | Identifier of string
  | Constructor of string
  | IntegerLiteral of int
  | BooleanLiteral of bool
  | Plus
  | Minus
  | Star
  | Period
  | Colon
  | Arrow
  | Lambda
  | OpenParen
  | ShutParen
  | Equal
  | Bar
  | Let
  | Recursive
  | In
  | Type
  | Of
  | Match
  | With
  | End
  | EOF
[@@deriving equal, show]
