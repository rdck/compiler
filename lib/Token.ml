open Core

type token =
  | Identifier of string
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
  | EOF
[@@deriving equal, show]
