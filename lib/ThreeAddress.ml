(******************************************************************************)
(* THREE ADDRESS CODE *)
(******************************************************************************)

open Core
open Symbol
open Types

type symbol = Apex.symbol
[@@deriving equal, show]

type binop = Syntax.binop
[@@deriving equal, show]

type register =
  | Reg of int
  | Arg
  | Env of identifier
[@@deriving equal, compare, sexp]

let show_register = function
  | Reg id -> sprintf "r%d" id
  | Arg -> "arg"
  | Env id -> sprintf "%s" id

let pp_register f r = Format.fprintf f "%s" (show_register r)

type expression =
  | Lit of int
  | Bin of binop * register * register
  | Closure of symbol * register list
  | Call of register * register
  | Con of identifier * register
  | Mat of register * ty * register list * symbol list
[@@deriving equal]

let show_expression = function
  | Lit i -> sprintf "%d" i
  | Bin (op, lhs, rhs) ->
      sprintf "%s %s %s" (Syntax.show_binop op) (show_register lhs) (show_register rhs)
  | Closure (f, args) ->
      let s = String.concat ~sep:" " (List.map args ~f:show_register) in
      sprintf "close f%d {%s}" f s
  | Call (f, x) ->
      sprintf "call %s %s" (show_register f) (show_register x)
  | Con (c, p) -> sprintf "%s %s" c (show_register p)
  | Mat (control, _, environment, cases) ->
      let show_symbol index = sprintf "f%d" index in
      let cases = String.concat ~sep:" | " (List.map cases ~f:show_symbol) in
      let environment = String.concat ~sep:" " (List.map environment ~f:show_register) in
      sprintf "match %s under [%s] with %s" (show_register control) environment cases

let pp_expression f e =
  Format.fprintf f "%s" (show_expression e)

type count_operation =
  | Inc
  | Dec
[@@deriving equal]

let show_count_operation = function
  | Inc -> "inc"
  | Dec -> "dec"

let pp_count_operation f op =
  Format.fprintf f "%s" (show_count_operation op)

type instruction =
  | Store of register * ty * expression
  | Return of register
  | Count of count_operation * register
[@@deriving equal]

let show_instruction = function
  | Store (out, t, value) ->
      sprintf "%s : %s := %s" (show_register out) ([%show: ty] t) (show_expression value)
  | Return r ->
      sprintf "ret %s" (show_register r)
  | Count (op, r) ->
      sprintf "%s %s" (show_count_operation op) (show_register r)

let pp_instruction f i =
  Format.fprintf f "%s" (show_instruction i)

type definition = {
  env : (identifier, ty) binding list ;
  arg : (identifier, ty) binding ;
  body : instruction list ;
  return_type : ty ;
}
[@@deriving equal]

let show_definition { env ; arg ; body ; return_type = _ } =
  let env' = [%show: (identifier, ty) binding list] env in
  let arg' = [%show: (identifier, ty) binding] arg in
  let ins = List.map body ~f:[%show: instruction] in
  sprintf "{%s} (%s) :=\n%s" env' arg' (String.concat ~sep:"\n" ins)

let pp_definition f d =
  Format.fprintf f "%s" (show_definition d)

type program = {
  types : (identifier, type_specifier) bindings ;
  terms : (symbol, definition) bindings ;
  body : instruction list ;
}

let show_program { types = _ ; terms ; body } =
  let f { name = k ; value = v } = sprintf "f%d %s" k (show_definition v) in
  let fs = List.map terms ~f in
  let body = List.map body ~f:show_instruction in
  let body = String.concat ~sep:"\n" body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n\n" fs) body

let pp_program f p =
  Format.fprintf f "%s" (show_program p)

let definition_type definition =
  Arrow (definition.arg.value, definition.return_type)

module Register = struct

  module T = struct

    type t = register
    [@@deriving compare, sexp]

  end

  include T
  include Comparable.Make(T)

end
