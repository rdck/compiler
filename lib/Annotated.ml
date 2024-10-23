(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Types
open Prelude

type identifier = STLC.identifier
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type pattern = {
  name : identifier ;
  parameter : identifier ;
  parameter_type : ty ;
}
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | App of 'a expression * 'a expression
  | Abs of identifier * 'a expression
  | Con of identifier * 'a expression
  | Mat of 'a expression * (pattern * 'a expression) list
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type value = ty expression
[@@deriving equal, show]

type program = {
  types : (identifier, type_specifier) bindings ;
  body : ty expression ;
}

(* TODO: factor out *)
let show_type_binding { name ; value } =
  sprintf "type %s = %s" name ([%show: type_specifier] value)

let show_program { types ; body } =
  let types = List.map types ~f:show_type_binding in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n" types) ""

let pp_program f program =
  Format.fprintf f "%s" (show_program program)
