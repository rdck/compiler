open Core
open Symbol

type ty =
  | TypeSymbol of identifier
  | Pointer of ty
[@@deriving equal, show]

type type_definition =
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
  | Alias of ty
[@@deriving equal, show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | LT
  | LEQ
  | GT
  | GEQ
[@@deriving equal, show]

type assignable =
  | Var of identifier
  | Arrow of assignable * identifier
  | Dot of assignable * identifier
[@@deriving equal, show]

type expression =
  | Assignable of assignable
  | Address of assignable
  | Lit of int
  | Call of identifier * expression list
  | Bin of binop * expression * expression
[@@deriving equal, show]

type statement =
  | Declare of identifier * ty
  | Assign of assignable * expression
  | Switch of expression * case list
  | Return of expression
  | Block of statement list
  | If of expression * statement
  | Effect of expression

and case =
  { tag : expression
  ; body : statement list
  }
[@@deriving equal, show]

type procedure =
  { args : (identifier, ty) bindings
  ; body : statement list
  ; return_type : ty
  }
[@@deriving equal, show]

type program =
  { types : (identifier, type_definition) bindings
  ; procedures : (identifier, procedure) bindings
  ; main : statement list
  }
[@@deriving equal, show]
