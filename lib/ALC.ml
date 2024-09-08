[@@@warning "-32"]
open Core

type identifier = STLC.identifier
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type 'a expression =
  | Lit of int
  | Bin of binop * 'a note * 'a note
  | Var of identifier
  | App of 'a note * 'a note
  | Abs of identifier * 'a note
and 'a note = {
  expr : 'a expression ;
  note : 'a ;
}
[@@deriving equal]

(* factor these out *)
let op_precedence = function
  | STLC.Add -> 2
  | STLC.Sub -> 2
  | STLC.Mul -> 3
let app_precedence = 4
let abs_precedence = 1
