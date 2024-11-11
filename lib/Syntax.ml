(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Types
open Symbol
include SyntaxData

let represent_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Exp -> "^"


let represent_pattern { name; parameter } = sprintf "%s %s" name parameter

module Expression = struct
  open PrettyPrinter

  type t = expression

  let structure = function
    | Lit _ -> Nullary
    | Bin (op, lhs, rhs) ->
      (match op with
       | Add -> Binary (2, Left, lhs, rhs)
       | Sub -> Binary (2, Left, lhs, rhs)
       | Mul -> Binary (3, Left, lhs, rhs)
       | Exp -> Binary (4, Right, lhs, rhs))
    | Var _ -> Nullary
    | App (f, x) -> Binary (5, Left, f, x)
    | Abs (_, body) -> Unary (1, body)
    (* unsure about this precedence *)
    | Con (_, arg) -> Unary (5, arg)
    (* TODO: figure out how to show patterns *)
    | Mat (e, es) -> Nary (e :: List.map es ~f:snd)
    | Rec (_, definition, body) -> Binary (6, Right, definition, body)


  let node_text = function
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> id
    | App _ -> " "
    | Abs ({ name; value = domain }, _) -> sprintf "λ %s : %s . " name (show_ty domain)
    | Con (id, _) -> sprintf "%s " id
    | Mat _ -> "match" (* incomplete *)
    | Rec ({ name; value = t }, _, _) -> sprintf " as %s : %s in " name (show_ty t)
end

module Printer = PrettyPrinter.Make (Expression)

let represent_expression = Printer.print

let represent_program { types; body } =
  let represent_type_binding { name; value } =
    sprintf "type %s = %s" name ([%show: type_specifier] value)
  in
  let types = List.map types ~f:represent_type_binding in
  let body = represent_expression body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n" types) body
