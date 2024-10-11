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
  | Z64
  | OpenParen
  | ShutParen
  | EOF
[@@deriving equal, show]
