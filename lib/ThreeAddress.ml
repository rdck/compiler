(******************************************************************************)
(* THREE ADDRESS CODE *)
(******************************************************************************)

open Core
open Symbol
open Types
include ThreeAddressData

let represent_binop = Apex.represent_binop

let represent_register = function
  | Reg id -> sprintf "r%d" id
  | Arg _ -> "arg"
  | Env id -> id
  | Loc id -> id


let represent_expression = function
  | Lit (IntegerLiteral i) -> sprintf "%d" i
  | Lit (BooleanLiteral b) -> sprintf "%b" b
  | Bin (op, lhs, rhs) ->
    sprintf
      "%s %s %s"
      (represent_binop op)
      (represent_register lhs)
      (represent_register rhs)
  | Closure (f, args) ->
    let s = String.concat ~sep:" " (List.map args ~f:represent_register) in
    sprintf "close f%d {%s}" f s
  | Call (f, x) -> sprintf "call %s %s" (represent_register f) (represent_register x)
  | Con (c, p) -> sprintf "%s %s" c (represent_register p)
  | Read r -> represent_register r


let represent_count_operation = function
  | Inc -> "inc"
  | Dec -> "dec"


let represent_instruction = function
  | Store (out, t, value) ->
    let out = represent_register out in
    let t = represent_ty t in
    let value = represent_expression value in
    sprintf "%s : %s := %s" out t value
  | Return r -> sprintf "ret %s" (represent_register r)
  | Count (op, r) -> sprintf "%s %s" (represent_count_operation op) (represent_register r)


let represent_ty_binding { name; value = t } = sprintf "%s : %s" name (represent_ty t)

let represent_definition { env; arg; body; return_type = _ } =
  let env = String.concat ~sep:", " @@ List.map env ~f:represent_ty_binding in
  let arg = represent_ty_binding arg in
  let ins = List.map body ~f:represent_instruction in
  sprintf "{%s} (%s) :=\n%s" env arg (String.concat ~sep:"\n" ins)


let represent_program { types = _; terms; body } =
  let f { name = k; value = v } = sprintf "f%d %s" k (represent_definition v) in
  let fs = List.map terms ~f in
  let body = List.map body ~f:represent_instruction in
  let body = String.concat ~sep:"\n" body in
  sprintf "%s\n\n%s" (String.concat ~sep:"\n\n" fs) body


let definition_env (d : definition) = d.env
let definition_arg (d : definition) = d.arg
let definition_body (d : definition) = d.body
let definition_return_type (d : definition) = d.return_type
let definition_type definition = Arrow (definition.arg.value, definition.return_type)

module Register = struct
  module T = struct
    type t = register [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end
