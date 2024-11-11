open Core
open Symbol
open Types

type binop = Syntax.binop
[@@deriving equal, show]

type pattern = Elaboration.pattern
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier (* from environment *)
  | Arg of identifier (* function parameter *)
  | Cls of symbol * 'a expression list
  | App of 'a expression * 'a expression
  | Con of identifier * 'a expression
  | Mat of 'a expression * 'a expression list * symbol list
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type term = ty expression
[@@deriving equal, show]

type definition = {
  env : (identifier, ty) bindings ;
  arg : (identifier, ty) binding ;
  body : term ;
}
[@@deriving equal, show]

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (symbol, definition) bindings ;
  body : term ;
}
[@@deriving equal, show]
