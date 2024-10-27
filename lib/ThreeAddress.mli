(******************************************************************************)
(* THREE ADDRESS CODE *)
(******************************************************************************)

open Core
open Prelude
open Types

type symbol = Apex.symbol
[@@deriving equal, show]

type identifier = Syntax.identifier
[@@deriving equal, show]

type binop = Syntax.binop
[@@deriving equal, show]

type register =
  | Reg of int
  | Arg
  | Env of identifier
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * register * register
  | Closure of symbol * register list
  | Call of register * register
[@@deriving equal, show]

type count_operation =
  | Inc
  | Dec
[@@deriving equal, show]

type instruction =
  | Store of register * ty * expression
  | Return of register
  | Count of count_operation * register
[@@deriving equal, show]

type definition = {
  env : (identifier, ty) bindings ;
  arg : (identifier, ty) binding ;
  body : instruction list ;
  return_type : ty ;
}
[@@deriving equal, show]

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (symbol, definition) bindings ;
  body : instruction list ;
}
[@@deriving show]

val definition_type : definition -> ty
