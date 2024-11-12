open Core
open Symbol
open Types
include ToponymData

let annotate e a = { expr = e; note = a }

module Term = struct
  open PrettyPrinter

  type t = term

  let structure { expr; note = _ } =
    match expr with
    | Lit _ -> Nullary
    | Bin (op, lhs, rhs) ->
      (match op with
       | Add -> Binary (2, Left, lhs, rhs)
       | Sub -> Binary (2, Left, lhs, rhs)
       | Mul -> Binary (3, Left, lhs, rhs)
       | Exp -> Binary (4, Right, lhs, rhs))
    | Var _ -> Nullary
    | Cls (_, args) -> Nary args
    | App (f, x) -> Binary (5, Left, f, x)
    | Con (_, p) -> Unary (5, p)
    | Mat _ -> Nullary (* TODO *)


  let node_text { expr; note = _ } =
    match expr with
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var (_, id) -> id
    | App _ -> " "
    | Cls (sym, _) -> sprintf "f%s" (represent_symbol sym)
    | Con (c, _) -> sprintf "%s " c
    | Mat _ -> "match" (* incomplete *)
end

module Printer = PrettyPrinter.Make (Term)

let represent_term = Printer.print
let represent_binop = Elaboration.represent_binop
let represent_pattern = Elaboration.represent_pattern
let represent_ty_binding { name; value = t } = sprintf "%s : %s" name (represent_ty t)

let represent_definition { env; arg; body } =
  let env = String.concat ~sep:", " @@ List.map env ~f:represent_ty_binding in
  let arg = represent_ty_binding arg in
  let body = represent_term body in
  sprintf "{%s} (%s) := %s" env arg body


let represent_program { types = _; terms; body } =
  let f { name = k; value = v } = sprintf "f%d %s" k (represent_definition v) in
  let fs = List.map terms ~f in
  let body = represent_term body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n\n" fs) body
