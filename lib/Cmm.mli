(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal]

type index = int
[@@deriving equal]

type 'a symbol_table = (identifier, 'a, String.comparator_witness) Map.t

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
  | Structure of (identifier, ty) binding list
  | Union of (identifier, ty) binding list
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
  types : ty symbol_table ;
  procedures : procedure symbol_table ;
  main : statement list ;
}

val represent : program -> string
