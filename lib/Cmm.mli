(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal]

type index = int
[@@deriving equal]

type expression =
  | Var of identifier
  | Arrow of expression * expression
  | Dot of expression * expression
  | Apply of expression
  | Binop of expression * expression

type ty =
  | Z64
  | TypeSymbol of identifier
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
[@@deriving equal]

type statement =
  | Declare of identifier * ty
  | Assign of expression * expression
  | Switch of expression * case list
  | Return of expression
and case = {
  tag : expression ;
  body : statement list ;
}

type procedure = {
  arg : (identifier, ty) binding ;
  body : statement list ;
}

type program = {
  types : (identifier, ty) bindings ;
  procedures : (identifier, procedure) bindings ;
  main : statement list ;
}

val represent : program -> string
