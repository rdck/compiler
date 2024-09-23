(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type ty =
  | Int
  | Arrow of ty * ty
[@@deriving equal]

let show_ty =
  let rec show p = function
    | Int -> "Z64"
    | Arrow (dom, cod) ->
        let dom' = show true dom in
        let cod' = show false cod in
        let s = sprintf "%s → %s" dom' cod' in
        if p then sprintf "(%s)" s else s in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal]

let show_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Exp -> "^"

let pp_binop f op = Format.fprintf f "%s" (show_binop op)

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of (identifier, ty) binding * expression
[@@deriving equal]

module Expression = struct

  open PrettyPrinter

  type t = expression

  let structure = function
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
  
  let node_text = function
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> id
    | App _ -> " "
    | Abs ({ name ; value = domain }, _) ->
        sprintf "λ %s : %s . " name (show_ty domain)

end

module Printer = PrettyPrinter.Make(Expression)

let show_expression = Printer.print

let pp_expression f e =
  Format.fprintf f "%s" (show_expression e)

let value_exn opt =
  Option.value_exn opt

let project_domain = function
  | Int -> None
  | Arrow (domain, _) -> Some domain

let project_domain_exn =
  Fn.compose value_exn project_domain

let project_codomain = function
  | Int -> None
  | Arrow (_, codomain) -> Some codomain 

let project_codomain_exn =
  Fn.compose value_exn project_codomain
