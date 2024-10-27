{

open Lexeme
open Core

exception UnexpectedCharacter of char

}

let digits = ['0'-'9']+
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let constructor = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let whitespace = [ ' ' '\n' '\t' '\r' ]+

rule lex = parse

  | whitespace { lex lexbuf }

  | "+"     { Plus        }
  | "-"     { Minus       }
  | "*"     { Star        }
  | "."     { Period      }
  | ":"     { Colon       }
  | "("     { OpenParen   }
  | ")"     { ShutParen   }
  | "\\"    { Lambda      }
  | "λ"     { Lambda      }
  | "->"    { Arrow       }
  | "→"     { Arrow       }
  | "="     { Equal       }
  | "|"     { Bar         }
  | "let"   { Let         }
  | "in"    { In          }
  | "type"  { Type        }
  | "of"    { Of          }
  | "match" { Match       }
  | "with"  { With        }
  | "end"   { End         }

  | digits as d { Literal (Int.of_string d) }
  | id as id { Identifier id }
  | constructor as id { Constructor id }
  | eof { EOF }
  | _ as c { raise (UnexpectedCharacter c) }

{

let tokenize input =

  let tokenize =
    let rec f tokens buffer =
      match lex buffer with
      | EOF -> EOF :: tokens
      | token -> f (token :: tokens) buffer
    in Fn.compose List.rev (f []) in

  try Result.return (tokenize input) with
  | UnexpectedCharacter c ->
      let message = sprintf "unexpected character: %c" c in
      Result.fail message

}
