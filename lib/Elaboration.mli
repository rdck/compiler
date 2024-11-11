(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Types
open Symbol

type binop = Syntax.binop
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
  (* by this time, patterns must be ordered by constructor *)
  | Mat of 'a expression * (pattern * 'a expression) list
  | Rec of identifier * 'a expression * 'a expression
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

(* curried expression constructor *)
val expression : 'a node -> 'a -> 'a expression

type program = {
  types : (identifier, type_specifier) bindings ;
  body : ty expression ;
}

val represent_program : program -> string
