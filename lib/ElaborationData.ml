open Core
open Symbol
open Types

type binop = Syntax.binop [@@deriving equal, show]

type pattern =
  { name : identifier
  ; parameter : identifier
  ; parameter_type : ty
  }
[@@deriving equal, show]

(* By this time, patterns must be ordered by constructor. *)
type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | App of 'a expression * 'a expression
  | Abs of identifier * 'a expression
  | Con of identifier * 'a expression
  | Mat of 'a expression * (pattern * 'a expression) list

and 'a expression =
  { expr : 'a node
  ; note : 'a
  }
[@@deriving equal, show]

type term = ty expression [@@deriving equal, show]

type program =
  { types : (identifier, type_specifier) bindings
  ; body : term
  }
[@@deriving equal, show]
