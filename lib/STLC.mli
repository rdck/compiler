(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

open Prelude

type identifier = string
[@@deriving equal, show]

type ty =
  | Int
  | Arrow of ty * ty
[@@deriving equal, show]

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of (identifier, ty) binding * expression
[@@deriving equal, show]

val project_domain    : ty -> ty option
val project_codomain  : ty -> ty option

val project_domain_exn    : ty -> ty
val project_codomain_exn  : ty -> ty
