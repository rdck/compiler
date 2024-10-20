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

