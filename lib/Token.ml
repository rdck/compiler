open Core

type token =
  | EOF
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
  | Identifier of string
  | Literal of int
[@@deriving equal]
