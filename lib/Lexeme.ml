open Core

type lexeme =
  | Identifier of string
  | Constructor of string
  | Literal of int
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
  | In
  | Type
  | Of
  | Match
  | With
  | End
  | EOF
[@@deriving equal, show]
