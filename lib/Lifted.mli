(******************************************************************************)
(* LIFTED LAMBDA CALCULUS *)
(******************************************************************************)

open Prelude
open Types

type binop = STLC.binop
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

(* A symbol is either... *)
type symbol =
  | Symbol of identifier        (* a given name         *)
  | GenSym of identifier * int  (* or a generated name. *)
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | Cls of symbol * 'a expression list
  | App of 'a expression * 'a expression
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type term = ty expression
[@@deriving equal, show]

type definition = {
  env : (identifier, ty) bindings ;
  arg : (identifier, ty) binding ;
  body : term ;
}
[@@deriving equal, show]

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (symbol, definition) bindings ;
}
[@@deriving show]
