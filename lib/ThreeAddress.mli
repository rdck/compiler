(******************************************************************************)
(* THREE ADDRESS CODE *)
(******************************************************************************)

open Core
open Symbol
open Types
include module type of ThreeAddressData

(* comparable registers *)
module Register : sig
  type t = register [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

(* curried closure constructor *)
val closure : symbol -> register list -> closure
val definition_env : definition -> (identifier, ty) bindings
val definition_arg : definition -> (identifier, ty) binding
val definition_body : definition -> instruction list
val definition_return_type : definition -> ty
val definition_type : definition -> ty
val represent_binop : binop -> string
val represent_register : register -> string
val represent_expression : expression -> string
val represent_instruction : instruction -> string
val represent_definition : definition -> string
val represent_program : program -> string
