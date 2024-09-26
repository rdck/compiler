(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type ty =
  | Z64
  | Arrow of ty * ty
[@@deriving equal, show, compare, sexp]

(* comparable types *)
module Ty : sig

  type t = ty [@@deriving compare, sexp]
  include Comparable.S with type t := t

end

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
