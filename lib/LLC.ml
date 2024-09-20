(******************************************************************************)
(* LIFTED LAMBDA CALCULUS                                                     *)
(******************************************************************************)

open Core

type binop = STLC.binop
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type 'a binding = 'a STLC.binding
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | Closure of index * expression list
  | App of expression * expression
[@@deriving equal, show]

type definition = {
  env : ty binding list ;
  arg : ty binding ;
  body : expression ;
}
[@@deriving equal, show]

type 'a symbol_table = (index, 'a, Int.comparator_witness) Map.t

type program = {
  functions : definition symbol_table ;
  body : expression ;
}
