open Core

type token =
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
  | Type
  | Match
  | End
  | Of
  | With
  | Def
  | EOF
[@@deriving equal, show]
