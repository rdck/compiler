(******************************************************************************)
(* SHARED TYPES *)
(******************************************************************************)

open Core
open Symbol
include module type of TypeData

(* comparable types *)
module Ty : sig
  type t = ty [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

val represent_ty : ty -> string
val represent_constructor : constructor -> string
val represent_type_specifier : type_specifier -> string
val z64 : ty
val z64_symbol : string
val b8 : ty
val b8_symbol : string
val ty_domain : ty -> ty option
val ty_codomain : ty -> ty option
val ty_domain_exn : ty -> ty
val ty_codomain_exn : ty -> ty
val ty_symbol : ty -> identifier option
val ty_symbol_exn : ty -> identifier
val is_symbol_type : ty -> bool
val is_arrow_type : ty -> bool
val is_heap_type : ty -> bool
