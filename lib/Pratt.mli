open Token
open Types

(* A parsing procedure will yield some syntax and the remaining lexemes. *)
type 'a parse = {
  syntax : 'a ;
  rest : token list ;
}

val project_syntax    : 'a parse -> 'a
val project_lexemes   : 'a parse -> token list

val pratt             : int -> token list -> (STLC.expression parse, string) result

val parse_type        : token list -> (ty parse, string) result
val parse_expression  : token list -> (STLC.expression parse, string) result
val parse_program     : token list -> (STLC.program, string) result
