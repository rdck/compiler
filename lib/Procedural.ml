(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Symbol
open Util
include ProceduralData

let rec represent_ty = function
  | TypeSymbol id -> id
  | Pointer t -> sprintf "%s*" (represent_ty t)


let represent_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | LT -> "<"
  | LEQ -> "<="
  | GT -> ">"
  | GEQ -> ">="


let rec represent_assignable =
  let represent = represent_assignable in
  function
  | Var id -> id
  | Arrow (a, id) -> sprintf "%s->%s" (represent a) id
  | Dot (a, id) -> sprintf "%s.%s" (represent a) id


let rec represent_expression =
  let represent = represent_expression in
  function
  | Assignable a -> represent_assignable a
  | Address a -> sprintf "&%s" (represent_assignable a)
  | Lit i -> sprintf "%dll" i
  | Call (id, args) -> sprintf "%s(%s)" id (concat_map args ~f:represent ~sep:", ")
  | Bin (op, lhs, rhs) ->
    sprintf "%s %s %s" (represent lhs) (represent_binop op) (represent rhs)


let rec represent_statement = function
  | Declare (id, t) -> sprintf "%s %s;" (represent_ty t) id
  | Assign (a, v) -> sprintf "%s = %s;" (represent_assignable a) (represent_expression v)
  | Switch (control, cases) ->
    sprintf
      "switch (%s) {\n%s\n}"
      (represent_expression control)
      (concat_map cases ~f:represent_case ~sep:"\n")
  | Block statements ->
    sprintf "{\n%s\n}" (concat_map statements ~f:represent_statement ~sep:"\n")
  | Return e -> sprintf "return %s;" (represent_expression e)
  | If (condition, body) ->
    sprintf "if (%s) %s" (represent_expression condition) (represent_statement body)
  | Effect e -> sprintf "%s;" (represent_expression e)


and represent_case { tag; body } =
  sprintf
    "case %s:\n{\n%s\n} break;"
    (represent_expression tag)
    (concat_map body ~f:represent_statement ~sep:"\n")


let declare { name; value } = sprintf "%s %s;" (represent_ty value) name

let represent_procedure { name; value = proc } =
  let represent_arg { name; value = t } = sprintf "%s %s" (represent_ty t) name in
  sprintf
    "%s %s(%s) {\n%s\n}"
    (represent_ty proc.return_type)
    name
    (concat_map proc.args ~f:represent_arg ~sep:", ")
    (concat_map proc.body ~f:represent_statement ~sep:"\n")


let prelude_items = [ "#include <stdint.h>"; "#include <stdlib.h>"; "#include <stdio.h>" ]

let represent_program { types; procedures; main } =
  let type_declaration name = function
    | Enumeration _ -> sprintf "typedef enum %s %s;" name name
    | Structure _ -> sprintf "typedef struct %s %s;" name name
    | Union _ -> sprintf "typedef union %s %s;" name name
    | Alias t -> sprintf "typedef %s %s;" (represent_ty t) name
  in
  let type_definition name = function
    | Enumeration ids -> sprintf "enum %s { %s };" name (String.concat ids ~sep:", ")
    | Structure bindings ->
      sprintf "struct %s { %s };" name (concat_map bindings ~f:declare ~sep:" ")
    | Union bindings ->
      sprintf "union %s { %s };" name (concat_map bindings ~f:declare ~sep:" ")
    | Alias _ -> ""
  in
  String.concat
    [ String.concat prelude_items ~sep:"\n"
    ; concat_map types ~sep:"\n" ~f:(fun { name; value } -> type_declaration name value)
    ; concat_map types ~sep:"\n" ~f:(fun { name; value } -> type_definition name value)
    ; concat_map procedures ~f:represent_procedure ~sep:"\n\n"
    ; sprintf
        "int64_t lambda_main() {\n%s\n}"
        (concat_map main ~f:represent_statement ~sep:"\n")
    ]
    ~sep:"\n\n"
