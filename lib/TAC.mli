(******************************************************************************)
(* THREE ADDRESS CODE *)
(******************************************************************************)

open Core
open Prelude

type index = int
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type register =
  | Reg of index
  | Arg
  | Env of identifier
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * register * register
  | Closure of index * register list
  | Call of register * register
[@@deriving equal, show]

type instruction =
  | Store of register * ty * expression
  | Return of register
[@@deriving equal, show]

type definition = {
  env : (identifier, ty) bindings ;
  arg : (identifier, ty) binding ;
  body : instruction list ;
  return_type : ty ;
}
[@@deriving equal, show]

type 'a symbol_table = (index, 'a, Int.comparator_witness) Map.t

type program = {
  functions : definition symbol_table ;
  body : instruction list ;
}
[@@deriving show]

val definition_type : definition -> ty
