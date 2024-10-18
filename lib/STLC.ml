(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, compare, sexp, show]

type ty =
  | TypeSymbol of identifier
  | Arrow of ty * ty
[@@deriving equal, compare, sexp]

let show_ty =
  let rec show p = function
    | TypeSymbol id -> id
    | Arrow (dom, cod) ->
        let dom' = show true dom in
        let cod' = show false cod in
        let s = sprintf "%s -> %s" dom' cod' in
        if p then sprintf "(%s)" s else s in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

let z64 = TypeSymbol "z64"

type constructor = {
  name : identifier ;
  parameter : ty ;
}
[@@deriving equal, show]

type type_body = constructor list
[@@deriving equal, show]

module Ty = struct

  module T = struct

    type t = ty
    [@@deriving compare, sexp]

  end

  include T
  include Comparable.Make(T)

end

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

type pattern = {
  name : identifier ;
  parameter : identifier ;
}
[@@deriving equal]

let show_pattern { name ; parameter } =
  sprintf "%s %s" name parameter

let pp_pattern f p =
  Format.fprintf f "%s" (show_pattern p)

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of (identifier, ty) binding * expression
  | Con of identifier * expression
  | Mat of expression * case list
and case = {
  pattern : pattern ;
  body : expression ;
}
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
    (* unsure about this precedence *)
    | Con (_, arg) -> Unary (5, arg)
    (* this would need to be n-ary *)
    | Mat _ -> Nullary
  
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
    | Con (id, _) -> id
    | Mat _ -> "match" (* incomplete *)

end

module Printer = PrettyPrinter.Make(Expression)

let show_expression = Printer.print

let pp_expression f e =
  Format.fprintf f "%s" (show_expression e)

let show_case { pattern ; body } =
  sprintf "%s => %s" (show_pattern pattern) (show_expression body)

let pp_case f c =
  Format.fprintf f "%s" (show_case c)

let project_domain = function
  | TypeSymbol _ -> None
  | Arrow (domain, _) -> Some domain

let project_domain_exn =
  Fn.compose value_exn project_domain

let project_codomain = function
  | TypeSymbol _ -> None
  | Arrow (_, codomain) -> Some codomain 

let project_codomain_exn =
  Fn.compose value_exn project_codomain

type program = {
  types : (identifier, type_body) bindings ;
  values : (identifier, expression) bindings ;
}
