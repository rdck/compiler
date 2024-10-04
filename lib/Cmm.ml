(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

let concat_map xs f sep =
  String.concat (List.map xs ~f:f) ~sep:sep

type binop =
  | Add
  | Sub
  | Mul

type expression =
  | Var of identifier
  | Lit of int
  | Arrow of expression * expression
  | Dot of expression * expression
  | Apply of expression * expression list
  | Bin of binop * expression * expression

type ty =
  | Z64
  | TypeSymbol of identifier
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
  | Pointer of ty
[@@deriving equal]

let rec show_ty = function
  | Z64 -> "int64_t"
  | TypeSymbol id -> id
  | Enumeration ids ->
      sprintf "enum { %s }" (String.concat ~sep:", " ids)
  | Structure bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      sprintf "struct { %s }" (concat_map bindings f " ")
  | Union bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      sprintf "union { %s }" (concat_map bindings f " ")
  | Pointer t -> sprintf "%s*" (show_ty t)

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type statement =
  | Declare of identifier * ty
  | Assign of expression * expression
  | Switch of expression * case list
  | Block of statement list
  | Return of expression
and case = {
  tag : expression ;
  body : statement list ;
}

let render_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let rec render_expression =
  let render = render_expression in function
    | Var id -> id
    | Lit i -> sprintf "%dll" i
    | Arrow (lhs, rhs) ->
        sprintf "%s->%s" (render lhs) (render rhs)
    | Dot (lhs, rhs) ->
        sprintf "%s.%s" (render lhs) (render rhs)
    | Apply (f, args) ->
        sprintf "%s(%s)" (render f) (concat_map args render ", ")
    | Bin (op, lhs, rhs) ->
        sprintf "%s %s %s" (render lhs) (render_binop op) (render rhs)

let rec render_statement = function
  | Declare (id, t) -> sprintf "%s %s;" (show_ty t) id
  | Assign (lvalue, rvalue) ->
      sprintf "%s = %s;" (render_expression lvalue) (render_expression rvalue)
  | Switch (control, cases) ->
      sprintf "switch (%s) {\n%s\n}" (render_expression control) (concat_map cases render_case "\n")
  | Block statements ->
      sprintf "{\n%s\n}" (concat_map statements render_statement "\n")
  | Return e ->
      sprintf "return %s;" (render_expression e)
and render_case { tag ; body } =
  sprintf "case %s:\n{\n%s\n} break;" (render_expression tag) (concat_map body render_statement "\n")

type procedure = {
  arg : (identifier, ty) binding ;
  body : statement list ;
  return_type : ty ;
}

type program = {
  types : (identifier, ty) bindings ;
  procedures : (identifier, procedure) bindings ;
  main : statement list ;
}

let represent_block statements = concat_map statements render_statement "\n"

let represent { types ; procedures ; main } =

  let typedef { name ; value } =
    sprintf "typedef %s %s;" ([%show: ty] value) name in

  let types' = concat_map types typedef "\n" in

  let procedure { name ; value = proc } =

    let argument { name ; value = t } =
      sprintf "%s %s" (show_ty t) name in

    sprintf "%s %s(%s) {\n%s}"
    (show_ty proc.return_type) name (argument proc.arg) (represent_block proc.body) in

  let procedures' = concat_map procedures procedure "\n" in

  String.concat [ types' ; procedures' ] ~sep:"\n\n"
