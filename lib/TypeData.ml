open Core
open Symbol

type ty =
  | TypeSymbol of identifier
  | Arrow of ty * ty
[@@deriving equal, show, compare, sexp]

type constructor =
  { name : identifier
  ; parameter : ty
  }
[@@deriving equal, show]

(* should have at least one constructor *)
type type_specifier = constructor list [@@deriving equal, show]
