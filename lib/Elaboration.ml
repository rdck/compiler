(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Types
open Symbol

include ElaborationData

let annotate expr note = { expr ; note }

(* TODO: This is too much of a duplicate of the printer module in Syntax, for my taste. *)
module Expression = struct

  open PrettyPrinter

  type t = ty expression

  let structure { expr ; note = _ } =
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
    | Rec (_, definition, body) -> Binary (6, Right, definition, body)
  
  let node_text { expr ; note = _ } =
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
    | Rec (id, _, _) ->
        sprintf " as %s in " id

end

module Printer = PrettyPrinter.Make(Expression)

let represent_expression = Printer.print

let represent_binop = Syntax.represent_binop

let represent_pattern { name ; parameter ; parameter_type } =
  sprintf "%s (%s : %s)" name parameter (represent_ty parameter_type)

let represent_program { types ; body } =

  let show_type_binding { name ; value } =
    sprintf "type %s = %s" name ([%show: type_specifier] value) in

  let types = List.map types ~f:show_type_binding in
  let body = represent_expression body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n" types) body
