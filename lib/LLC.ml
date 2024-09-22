(******************************************************************************)
(* LIFTED LAMBDA CALCULUS                                                     *)
(******************************************************************************)

open Core

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

type 'a expression =
  | Lit of int
  | Bin of binop * 'a note * 'a note
  | Var of identifier
  | Closure of index * 'a note list
  | App of 'a note * 'a note
and 'a note = {
  expr : 'a expression ;
  note : 'a ;
}
[@@deriving equal, show]

type term = ty note
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
    | Closure (_, args) -> Nary args
    | App (f, x) -> Binary (5, Left, f, x)

  let node_text { expr ; note = _ } =
    match expr with
    | Lit i -> sprintf "%d" i
    | Bin (Add, _, _) -> " + "
    | Bin (Sub, _, _) -> " - "
    | Bin (Mul, _, _) -> " * "
    | Bin (Exp, _, _) -> " ^ "
    | Var id -> id
    | App _ -> " "
    | Closure (idx, _) -> sprintf "f%d" idx

end

module Printer = PrettyPrinter.Make(Term)

let show_term = Printer.print

let pp_term f e = Format.fprintf f "%s" (show_term e)

type definition = {
  env : ty binding list ;
  arg : ty binding ;
  body : term ;
}
[@@deriving equal]

let show_definition { env ; arg ; body } =
  let env' = [%show: ty binding list] env in
  let arg' = [%show: ty binding] arg in
  let body' = [%show: term] body in
  sprintf "{%s} (%s) := %s" env' arg' body'

let pp_definition f d = Format.fprintf f "%s" (show_definition d)

type 'a symbol_table = (index, 'a, Int.comparator_witness) Map.t

type program = {
  functions : definition symbol_table ;
  body : term ;
}

let show_program { functions ; body } =
  let fs = Map.to_alist functions in
  let f (k, v) = sprintf "f%d %s" k (show_definition v) in
  let fs' = List.map fs ~f:f in
  sprintf "%s\n%s" (String.concat ~sep:"\n" fs') (show_term body)

let pp_program f p =
  Format.fprintf f "%s" (show_program p)
