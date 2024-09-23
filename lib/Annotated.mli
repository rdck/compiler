(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Core

type identifier = string
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | App of 'a expression * 'a expression
  | Abs of identifier * 'a expression
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]
