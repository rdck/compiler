(******************************************************************************)
(* SHARED TYPES *)
(******************************************************************************)

open Core

type identifier = string
[@@deriving equal, show]

type ty =
  | TypeSymbol of identifier
  | Arrow of ty * ty
[@@deriving equal, show, compare, sexp]

val z64 : ty
val z64_symbol : string

type constructor = {
  name : identifier ;
  parameter : ty ;
}
[@@deriving equal, show]

(* should have at least one constructor *)
type type_specifier = constructor list
[@@deriving equal, show]

(* comparable types *)
module Ty : sig

  type t = ty [@@deriving compare, sexp]
  include Comparable.S with type t := t

end

val project_domain    : ty -> ty option
val project_codomain  : ty -> ty option

val project_domain_exn    : ty -> ty
val project_codomain_exn  : ty -> ty

val is_symbol_type  : ty -> bool
val is_arrow_type   : ty -> bool
