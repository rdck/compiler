(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Symbol

include ProceduralData

(* TODO: pull out *)
let concat_map xs f sep =
  String.concat (List.map xs ~f:f) ~sep:sep

let rec represent_ty = function
  | TypeSymbol id -> id
  | Pointer t -> sprintf "%s*" (represent_ty t)

let represent_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | LT  -> "<"
  | LEQ -> "<="
  | GT  -> ">"
  | GEQ -> ">="

let rec represent_assignable =
  let represent = represent_assignable in function
    | Var id -> id
    | Arrow (a, id) -> sprintf "%s->%s" (represent a) id
    | Dot (a, id) -> sprintf "%s.%s" (represent a) id

let rec represent_expression =
  let represent = represent_expression in function
    | Assignable a -> represent_assignable a
    | Address a -> sprintf "&%s" (represent_assignable a)
    | Lit i -> sprintf "%dll" i
    | Call (id, args) ->
        sprintf "%s(%s)" id (concat_map args represent ", ")
    | Bin (op, lhs, rhs) ->
        sprintf "%s %s %s" (represent lhs) (represent_binop op) (represent rhs)

let rec represent_statement = function
  | Declare (id, t) -> sprintf "%s %s;" (represent_ty t) id
  | Assign (a, v) ->
      sprintf "%s = %s;" (represent_assignable a) (represent_expression v)
  | Switch (control, cases) ->
      let cases = concat_map cases represent_case "\n" in
      sprintf "switch (%s) {\n%s\n}" (represent_expression control) cases
  | Block statements ->
      sprintf "{\n%s\n}" (concat_map statements represent_statement "\n")
  | Return e ->
      sprintf "return %s;" (represent_expression e)
  | If (condition, body) ->
      sprintf "if (%s) %s" (represent_expression condition) (represent_statement body)
  | Effect e -> sprintf "%s;" (represent_expression e)
and represent_case { tag ; body } =
  let body' = concat_map body represent_statement "\n" in
  sprintf "case %s:\n{\n%s\n} break;" (represent_expression tag) body'

let type_declaration name = function
  | Enumeration _ -> sprintf "typedef enum %s %s;" name name
  | Structure _ -> sprintf "typedef struct %s %s;" name name
  | Union _ -> sprintf "typedef union %s %s;" name name
  | Alias t -> sprintf "typedef %s %s;" (represent_ty t) name

let declare { name ; value } =
  sprintf "%s %s;" (represent_ty value) name

let type_definition name = function
  | Enumeration ids ->
      sprintf "enum %s { %s };" name (String.concat ids ~sep:", ")
  | Structure bindings ->
      sprintf "struct %s { %s };" name (concat_map bindings declare " ")
  | Union bindings ->
      sprintf "union %s { %s };" name (concat_map bindings declare " ")
  | Alias _ -> ""

let represent_procedure { name ; value = proc } =

  let represent_arg { name ; value = t } =
    sprintf "%s %s" (represent_ty t) name in

  sprintf "%s %s(%s) {\n%s\n}"
  (represent_ty proc.return_type)
  name
  (concat_map proc.args represent_arg ", ")
  (concat_map proc.body represent_statement "\n")

let represent_program { types ; procedures ; main } =

  let prelude_items = [
    "#include <stdint.h>" ;
    "#include <stdlib.h>" ;
    "#include <stdio.h>" ;
  ] in
  let prelude = String.concat prelude_items ~sep:"\n" in
  let type_declaration' { name ; value } = type_declaration name value in
  let type_definition' { name ; value } = type_definition name value in
  let type_declarations = concat_map types type_declaration' "\n" in
  let type_definitions = concat_map types type_definition' "\n" in
  let procedures' = concat_map procedures represent_procedure "\n\n" in
  let main_body = concat_map main represent_statement "\n" in
  let main' = sprintf "int64_t lambda_main() {\n%s\n}" main_body in
  String.concat [
    prelude ;
    type_declarations ;
    type_definitions ;
    procedures' ;
    main' ;
] ~sep:"\n\n"
