open Core
open Prelude
open Token
open Types
open STLC
open Result.Let_syntax

let fail = Result.fail
let fail_format fmt = fail (sprintf fmt)

type 'a parse = {
  syntax : 'a ;
  rest : token list ;
}

(* projection functions *)
let project_syntax  { syntax ; rest = _ } = syntax
let project_lexemes { syntax = _ ; rest } = rest

(* curried parse construction *)
let parse syntax rest = { syntax ; rest }

(* return a parse in the result monad *)
let return_parse syntax rest = return (parse syntax rest)

(* apply a function to the syntax of a parse *)
let parse_map f { syntax ; rest } =
  { syntax = f syntax ; rest }

(* consume a token without using it *)
let consume expect = function
  (* TODO: better representation of tokens *)
  | t :: ts when [%equal: token] t expect -> return ts
  | _ -> fail (sprintf "expected token: %s" ([%show: token] expect))

let rec parse_type tokens =

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { syntax ; rest } = parse_type rest in
        let%bind rest = consume ShutParen rest in
        return_parse syntax rest
    | Identifier id :: rest -> return_parse (TypeSymbol id) rest
    | _ -> fail "expected type atom" in

  let%bind { syntax = lhs ; rest } = parse_atom tokens in
  let rhs = let%bind rest = consume Arrow rest in parse_type rest in
  match rhs with
  | Error _ -> return_parse lhs rest
  | Ok { syntax = rhs ; rest } -> return_parse (Arrow (lhs, rhs)) rest

let parse_pattern = function
  | Constructor c :: Identifier p :: rest ->
      return_parse { name = c ; parameter = p } rest
  | _ -> fail "expected pattern"

let rec pratt p tokens =

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { syntax ; rest } = pratt 0 rest in
        let%bind rest = consume ShutParen rest in
        return_parse syntax rest
    | Constructor id :: rest ->
        let%bind { syntax ; rest } = pratt 5 rest in
        return_parse (Con (id, syntax)) rest
    (* TODO: handle case of no cases *)
    | Match :: rest ->
        let%bind { syntax = control ; rest } = pratt 0 rest in
        let%bind rest = consume With rest in
        let { syntax = cases ; rest } = parse_cases rest in
        let%bind rest = consume End rest in
        return_parse (Mat (control, cases)) rest
    | Identifier id :: rest -> return_parse (Var id) rest
    | Literal l :: rest -> return_parse (Lit l) rest
    | _ -> fail "expected atom" in

  match tokens with
  | Lambda :: Identifier id :: Colon :: rest ->
      let%bind { syntax = t ; rest } = parse_type rest in
      let%bind rest = consume Period rest in
      let%bind { syntax = e ; rest } = pratt 1 rest in
      return_parse (Abs ({ name = id ; value = t }, e)) rest
  | _ ->
      let%bind { syntax = lhs ; rest } = parse_atom tokens in
      let { syntax ; rest } = loop p lhs rest in
      return_parse syntax rest

and loop p lhs rest =

  let default = { syntax = lhs ; rest } in

  (* assumes left associativity *)
  let fold f p' rest =
    match p' > p with
    | true ->
        let looped =
          let%bind { syntax = rhs ; rest } = pratt p' rest in
          return @@ loop p (f lhs rhs) rest in
        begin match looped with
        | Error _ -> default
        | Ok ok -> ok
        end
    | false -> default in

  match rest with
  | Plus :: tokens ->
      let f lhs rhs = Bin (Add, lhs, rhs) in
      fold f 2 tokens
  | Minus :: tokens ->
      let f lhs rhs = Bin (Sub, lhs, rhs) in
      fold f 2 tokens
  | Star :: tokens ->
      let f lhs rhs = Bin (Mul, lhs, rhs) in
      fold f 3 tokens
  | _ ->
      let f lhs rhs = App (lhs, rhs) in
      fold f 5 rest

and parse_cases rest =

  let parse_case rest =
    let%bind rest = consume Bar rest in
    let%bind { syntax = pattern ; rest } = parse_pattern rest in
    let%bind rest = consume Arrow rest in
    let%bind { syntax = body ; rest } = pratt 0 rest in
    return_parse (pattern, body) rest in

  let rec parse_cases rest =
    match parse_case rest with
    | Error _ -> parse [] rest
    | Ok { syntax ; rest } ->
        let { syntax = results ; rest } = parse_cases rest in
        parse (syntax :: results) rest in

  let { syntax ; rest } = parse_cases rest in
  parse_map List.rev (parse_cases rest)

let parse_expression = pratt 0

let parse_constructor = function
  | Bar :: Constructor name :: Of :: rest ->
      let%bind { syntax = parameter ; rest } = parse_type rest in
      return_parse Types.{ name ; parameter } rest
  | _ -> fail "expected constructor"

(* TODO: disallow empty list *)
let rec parse_type_specifier rest =
  match parse_constructor rest with
  | Error _ -> { syntax = [] ; rest }
  | Ok { syntax = c ; rest } ->
      let { syntax = cs ; rest } = parse_type_specifier rest in
      parse (c :: cs) rest

let parse_type_definition rest =
  match rest with
  | Type :: Identifier id :: Equal :: rest ->
      let { syntax = spec ; rest } = parse_type_specifier rest in
      return_parse (binding id spec) rest
  | _ -> fail "expected type definition"

let rec parse_type_definitions rest =
  match parse_type_definition rest with
  | Error _ -> { syntax = [] ; rest }
  | Ok { syntax = def ; rest } ->
      let { syntax = defs ; rest } = parse_type_definitions rest in
      parse (def :: defs) rest

let parse_program rest =
  let { syntax = types ; rest } = parse_type_definitions rest in
  let%bind { syntax = body ; rest } = parse_expression rest in
  let%bind _ = consume EOF rest in
  return { types ; body ; }
