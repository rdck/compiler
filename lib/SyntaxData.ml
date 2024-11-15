open Core
open Symbol
open Types

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal, show]

(* We'll make this recursive later. *)
type pattern =
  { name : identifier
  ; parameter : identifier
  }
[@@deriving equal, show]

type literal =
  | IntegerLiteral of int
  | BooleanLiteral of bool
[@@deriving equal, show]

type expression =
  | Lit of literal
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of (identifier, ty) binding * expression
  | Con of identifier * expression
  | Mat of expression * (pattern * expression) list
  | Let of identifier * expression * expression
[@@deriving equal, show]

type program =
  { types : (identifier, type_specifier) bindings
  ; body : expression
  }
[@@deriving equal, show]
