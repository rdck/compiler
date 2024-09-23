(******************************************************************************)
(* LIFTED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Prelude

type binop = STLC.binop
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | Closure of index * 'a expression list
  | App of 'a expression * 'a expression
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type term = ty expression
[@@deriving equal, show]

type definition = {
  env : (STLC.identifier, ty) binding list ;
  arg : (STLC.identifier, ty) binding ;
  body : term ;
}
[@@deriving equal, show]

type 'a symbol_table = (index, 'a, Int.comparator_witness) Map.t

type program = {
  functions : definition symbol_table ;
  body : term ;
}
[@@deriving show]
