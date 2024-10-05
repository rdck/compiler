(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type ty =
  | TypeSymbol of identifier
  | Pointer of ty
[@@deriving equal]

type type_definition =
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
  | Alias of ty
[@@deriving equal]

type binop =
  | Add
  | Sub
  | Mul
  | Div
[@@deriving equal]

type assignable =
  | Var of identifier
  | Arrow of assignable * identifier
  | Dot of assignable * identifier
[@@deriving equal]

type expression =
  | Assignable of assignable
  | Lit of int
  | Call of identifier * expression list
  | Bin of binop * expression * expression
[@@deriving equal]

type statement =
  | Declare of identifier * ty
  | Assign of assignable * expression
  | Switch of expression * case list
  | Return of expression
  | Block of statement list
and case = {
  tag : expression ;
  body : statement list ;
}
[@@deriving equal]

type procedure = {
  args : (identifier, ty) bindings ;
  body : statement list ;
  return_type : ty ;
}

type program = {
  types : (identifier, type_definition) bindings ;
  procedures : (identifier, procedure) bindings ;
  main : statement list ;
}

let concat_map xs f sep =
  String.concat (List.map xs ~f:f) ~sep:sep

let rec render_ty = function
  | TypeSymbol id -> id
  | Pointer t -> sprintf "%s*" (render_ty t)
(*
  | Enumeration ids ->
      sprintf "enum { %s }" (String.concat ~sep:", " ids)
  | Structure bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      sprintf "struct { %s }" (concat_map bindings f " ")
  | Union bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      sprintf "union { %s }" (concat_map bindings f " ")
  | Pointer t -> sprintf "%s*" (show_ty t)
*)

let render_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let rec render_assignable =
  let render = render_assignable in function
    | Var id -> id
    | Arrow (a, id) -> sprintf "%s->%s" (render a) id
    | Dot (a, id) -> sprintf "%s.%s" (render a) id

let rec render_expression =
  let render = render_expression in function
    | Assignable a -> render_assignable a
    | Lit i -> sprintf "%dll" i
    | Call (id, args) ->
        sprintf "%s(%s)" id (concat_map args render ", ")
    | Bin (op, lhs, rhs) ->
        sprintf "%s %s %s" (render lhs) (render_binop op) (render rhs)

let rec render_statement = function
  | Declare (id, t) -> sprintf "%s %s;" (render_ty t) id
  | Assign (a, v) ->
      sprintf "%s = %s;" (render_assignable a) (render_expression v)
  | Switch (control, cases) ->
      let cases' = concat_map cases render_case "\n" in
      sprintf "switch (%s) {\n%s\n}" (render_expression control) cases'
  | Block statements ->
      sprintf "{\n%s\n}" (concat_map statements render_statement "\n")
  | Return e ->
      sprintf "return %s;" (render_expression e)
and render_case { tag ; body } =
  let body' = concat_map body render_statement "\n" in
  sprintf "case %s:\n{\n%s\n} break;" (render_expression tag) body'

(* factor out? *)
let type_declaration name = function
  | Enumeration _ -> sprintf "typedef enum %s %s;" name name
  | Structure _ -> sprintf "typedef struct %s %s;" name name
  | Union _ -> sprintf "typedef union %s %s;" name name
  | Alias t -> sprintf "typedef %s %s;" (render_ty t) name

let declare { name ; value } =
  sprintf "%s %s;" (render_ty value) name

let type_definition name = function
  | Enumeration ids ->
      sprintf "enum %s { %s };" name (String.concat ids ~sep:", ")
  | Structure bindings ->
      sprintf "struct %s { %s };" name (concat_map bindings declare " ")
  | Union bindings ->
      sprintf "union %s { %s };" name (concat_map bindings declare " ")
  | Alias t -> ""

let render_procedure { name ; value = proc } =

  let render_arg { name ; value = t } =
    sprintf "%s %s" (render_ty t) name in

  sprintf "%s %s(%s) {\n%s\n}"
  (render_ty proc.return_type)
  name
  (concat_map proc.args render_arg ", ")
  (concat_map proc.body render_statement "\n")

let represent { types ; procedures ; main } =

  let type_declaration' { name ; value } = type_declaration name value in
  let type_definition' { name ; value } = type_definition name value in
  let type_declarations = concat_map types type_declaration' "\n" in
  let type_definitions = concat_map types type_definition' "\n" in
  let procedures' = concat_map procedures render_procedure "\n\n" in
  String.concat [
    type_declarations ;
    type_definitions ;
    procedures' ;
] ~sep:"\n\n"
