open Core
open Symbol
open Types

type binop = Syntax.binop [@@deriving equal, show]
type literal = Syntax.literal [@@deriving equal, show]

type register =
  | Reg of int
  | Arg of identifier
  | Env of identifier
  | Loc of identifier
[@@deriving equal, show, compare, sexp]

type expression =
  | Lit of literal
  | Bin of binop * register * register
  | Closure of closure
  | Call of register * register
  | Con of identifier * register
  | Mat of register * closure list
  | Read of register

and closure =
  { code : symbol
  ; data : register list
  }
[@@deriving equal, show]

type count_operation =
  | Inc
  | Dec
[@@deriving equal, show]

type instruction =
  | Store of register * ty * expression
  | Return of register
  | Count of count_operation * register
[@@deriving equal, show]

type definition =
  { env : (identifier, ty) bindings
  ; arg : (identifier, ty) binding
  ; body : instruction list
  ; return_type : ty
  }
[@@deriving equal, show]

type program =
  { types : (identifier, type_specifier) bindings
  ; terms : (symbol, definition) bindings
  ; body : instruction list
  }
[@@deriving equal, show]
