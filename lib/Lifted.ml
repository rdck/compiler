(******************************************************************************)
(* LIFTED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Prelude
open Types

type binop = STLC.binop
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

type symbol = int
[@@deriving equal, show]

type 'a node =
  | Lit of int
  | Bin of binop * 'a expression * 'a expression
  | Var of identifier
  | Arg of identifier
  | Cls of symbol * 'a expression list
  | App of 'a expression * 'a expression
and 'a expression = {
  expr : 'a node ;
  note : 'a ;
}
[@@deriving equal, show]

type term = ty expression
[@@deriving equal]

module Term = struct

  open PrettyPrinter

  type t = term

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
    | Arg _ -> Nullary
    | Cls (_, args) -> Nary args
    | App (f, x) -> Binary (5, Left, f, x)

  let node_text { expr ; note = _ } =
    match expr with
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> sprintf "env.%s" id
    | Arg id -> id
    | App _ -> " "
    | Cls (sym, _) -> show_symbol sym

end

module Printer = PrettyPrinter.Make(Term)

let show_term = Printer.print

let pp_term f e = Format.fprintf f "%s" (show_term e)

type definition = {
  env : (identifier, ty) binding list ;
  arg : (identifier, ty) binding ;
  body : term ;
}
[@@deriving equal]

let show_definition { env ; arg ; body } =
  let env' = [%show: (identifier, ty) binding list] env in
  let arg' = [%show: (identifier, ty) binding] arg in
  let body' = [%show: term] body in
  sprintf "{%s} (%s) := %s" env' arg' body'

let pp_definition f d = Format.fprintf f "%s" (show_definition d)

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (symbol, definition) bindings ;
  body : term ;
}

let show_program { types ; terms ; body } =
  let f { name = k ; value = v } = sprintf "f%d %s" k (show_definition v) in
  let fs = List.map terms ~f in
  let body = show_term body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n\n" fs) body

let pp_program f p =
  Format.fprintf f "%s" (show_program p)
