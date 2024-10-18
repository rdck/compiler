(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS *)
(******************************************************************************)

open Prelude
open Types

type identifier = string
[@@deriving equal, show]

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal, show]

(* We'll make this recursive later. *)
type pattern = {
  name : identifier ;
  parameter : identifier ;
}
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of (identifier, ty) binding * expression
  | Con of identifier * expression
  | Mat of expression * case list
and case = {
  pattern : pattern ;
  body : expression ;
}
[@@deriving equal, show]

val project_domain    : ty -> ty option
val project_codomain  : ty -> ty option

val project_domain_exn    : ty -> ty
val project_codomain_exn  : ty -> ty

type program = {
  types : (identifier, type_specifier) bindings ;
  values : (identifier, expression) bindings ;
}
