open Lexeme
open Types

(* A parsing procedure will yield some syntax and the remaining lexemes. *)
type 'a parse = {
  syntax : 'a ;
  rest : lexeme list ;
}

val project_syntax    : 'a parse -> 'a
val project_lexemes   : 'a parse -> lexeme list

val pratt             : int -> lexeme list -> (Syntax.expression parse, string) result

val parse_type        : lexeme list -> (ty parse, string) result
val parse_expression  : lexeme list -> (Syntax.expression parse, string) result
val parse_program     : lexeme list -> (Syntax.program, string) result
