(******************************************************************************)
(* LIFTED LAMBDA CALCULUS                                                     *)
(******************************************************************************)

open Core

let sprintf = Printf.sprintf

type binop = STLC.binop
[@@deriving equal, show]

type identifier = STLC.identifier
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type 'a binding = 'a STLC.binding
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | Closure of index * expression list
  | App of expression * expression
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
    | Closure (_, args) -> Nary args
    | App (f, x) -> Binary (5, Left, f, x)

  let node_text = function
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> id
    | App _ -> " "
    | Closure (idx, _) -> sprintf "f%d" idx

end

module Printer = PrettyPrinter.Make(Expression)

let show_expression = Printer.print

let pp_expression f e = Format.fprintf f "%s" (show_expression e)

type definition = {
  env : ty binding list ;
  arg : ty binding ;
  body : expression ;
}
[@@deriving equal]

let show_definition { env ; arg ; body } =
  let env' = [%show: ty binding list] env in
  let arg' = [%show: ty binding] arg in
  let body' = [%show: expression] body in
  sprintf "{%s} (%s) := %s" env' arg' body'

let pp_definition f d = Format.fprintf f "%s" (show_definition d)

type 'a symbol_table = (index, 'a, Int.comparator_witness) Map.t

type program = {
  functions : definition symbol_table ;
  body : expression ;
}

let show_program { functions ; body } =
  let fs = Map.to_alist functions in
  let f (k, v) = sprintf "f%d %s" k (show_definition v) in
  let fs' = List.map fs ~f:f in
  sprintf "%s\n%s" (String.concat ~sep:"\n" fs') (show_expression body)

let pp_program f p =
  Format.fprintf f "%s" (show_program p)
