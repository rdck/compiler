(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Types
open Prelude

type identifier = Syntax.identifier
[@@deriving equal, show]

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
  | Mat of 'a expression * (pattern * 'a expression) list
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

let expression expr note = { expr ; note }

(* TODO: This is too much of a duplicate of the printer module in Syntax, for my taste. *)
module Expression = struct

  open PrettyPrinter

  type t = ty expression

  let structure { expr ; note } =
    match expr with
    | Lit _ -> Nullary
    | Bin (op, lhs, rhs) ->
        begin match op with
        | Add -> Binary (2, Left , lhs, rhs)
        | Sub -> Binary (2, Left , lhs, rhs)
        | Mul -> Binary (3, Left , lhs, rhs)
        | Exp -> Binary (4, Right, lhs, rhs)
        end
    | Var _ -> Nullary
    | App (f, x) -> Binary (5, Left, f, x)
    | Abs (_, body) -> Unary (1, body)
    (* unsure about this precedence *)
    | Con (_, arg) -> Unary (5, arg)
    (* TODO: figure out how to show patterns *)
    | Mat (e, es) -> Nary (e :: List.map es ~f:snd)
  
  let node_text { expr ; note } =
    match expr with
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> id
    | App _ -> " "
    | Abs (id, _) ->
        sprintf "λ %s . " id
    | Con (id, _) -> sprintf "%s " id
    | Mat _ -> "match" (* incomplete *)

end

module Printer = PrettyPrinter.Make(Expression)

type program = {
  types : (identifier, type_specifier) bindings ;
  body : ty expression ;
}

(* TODO: factor out *)
let show_type_binding { name ; value } =
  sprintf "type %s = %s" name ([%show: type_specifier] value)

let represent_program { types ; body } =
  let types = List.map types ~f:show_type_binding in
  let body = Printer.print body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n" types) body
