(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal]

type index = int
[@@deriving equal]

type ty =
  | TypeSymbol of identifier
  | Pointer of ty
[@@deriving equal]

type type_definition =
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
  | Alias of ty
[@@deriving equal]

type binop =
  | Add
  | Sub
  | Mul
  | Div
[@@deriving equal]

type assignable =
  | Var of identifier
  | Arrow of assignable * identifier
  | Dot of assignable * identifier
[@@deriving equal]

type expression =
  | Assignable of assignable
  | Lit of int
  | Call of identifier * expression list
  | Bin of binop * expression * expression
[@@deriving equal]

type statement =
  | Declare of identifier * ty
  | Assign of assignable * expression
  | Switch of expression * case list
  | Return of expression
  | Block of statement list
and case = {
  tag : expression ;
  body : statement list ;
}
[@@deriving equal]

type procedure = {
  args : (identifier, ty) bindings ;
  body : statement list ;
  return_type : ty ;
}

type program = {
  types : (identifier, type_definition) bindings ;
  procedures : (identifier, procedure) bindings ;
  main : statement list ;
}

val represent : program -> string
