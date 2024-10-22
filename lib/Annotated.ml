(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Types
open Prelude

type identifier = STLC.identifier
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type pattern = STLC.pattern
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | App of 'a expression * 'a expression
  | Abs of identifier * 'a expression
  | Con of identifier * 'a expression
  | Mat of 'a expression * (pattern * 'a expression) list
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type value = ty expression
[@@deriving equal, show]

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (identifier, value) bindings ;
}
