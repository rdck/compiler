{
open Printf
open Token
open Core
}

let digits = ['0'-'9']+
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let whitespace = [ ' ' '\n' '\t' '\r' ]+

rule lex = parse
  | whitespace { lex lexbuf }
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Star }
  | "." { Period }
  | ":" { Colon }
  | "(" { OpenParen }
  | ")" { ShutParen }
  | "\\" { Lambda }
  | "->" { Arrow }
  | "Z64" { Z64 }
  | digits as d { Literal (Int.of_string d) }
  | id as id { Identifier id }
  | eof { EOF }

{
let tokenize =
  let rec f tokens buf =
    match lex buf with
    | EOF -> tokens
    | token -> f (token :: tokens) buf
  in Fn.compose List.rev (f [])
}
