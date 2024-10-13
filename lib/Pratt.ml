open Core
open Token

module T = STLC (* target *)

type 't parse = {
  result : 't ;
  rest : token list ;
}

let parse result rest = { result ; rest }
let return_parse result rest = Option.return (parse result rest)

let consume token = function
  | t :: ts -> if [%equal:token] t token then Some ts else None
  | _ -> None

let rec parse_type tokens =

  let open Option.Let_syntax in

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { result ; rest } = parse_type rest in
        let%bind rest = consume ShutParen rest in
        return_parse result rest
    | Z64 :: rest -> return_parse T.Z64 rest
    | _ -> None in

  let%bind { result = lhs ; rest } = parse_atom tokens in
  let rhs = let%bind rest = consume Arrow rest in parse_type rest in
  Option.value_map rhs ~default:(return_parse lhs rest) ~f:(
    fun { result = rhs ; rest } -> return_parse (T.Arrow (lhs, rhs)) rest
  )

let rec pratt p tokens =

  let open Option.Let_syntax in

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { result ; rest } = pratt 0 rest in
        let%bind rest = consume ShutParen rest in
        return_parse result rest
    | Identifier id :: rest -> return_parse (T.Var id) rest
    | Literal l :: rest -> return_parse (T.Lit l) rest
    | _ -> None in

  match tokens with
  | Lambda :: Identifier id :: Colon :: rest ->
      let%bind { result = t ; rest } = parse_type rest in
      let%bind rest = consume Period rest in
      let%bind { result = e ; rest } = pratt 1 rest in
      return_parse T.(Abs ({ name = id ; value = t }, e)) rest
  | _ ->
      let%bind { result = lhs ; rest } = parse_atom tokens in
      let { result ; rest } = loop p lhs rest in
      return_parse result rest

and loop p lhs rest =

  let open Option.Let_syntax in
  let default = { result = lhs ; rest } in

  (* assumes left associativity *)
  let fold f p' rest =
    match p' > p with
    | true ->
        let looped =
          let%bind { result = rhs ; rest } = pratt p' rest in
          return @@ loop p (f lhs rhs) rest in
        Option.value looped ~default
    | false -> default in

  match rest with
  | Plus :: tokens ->
      let f lhs rhs = T.(Bin (Add, lhs, rhs)) in
      fold f 2 tokens
  | Minus :: tokens ->
      let f lhs rhs = T.(Bin (Sub, lhs, rhs)) in
      fold f 2 tokens
  | Star :: tokens ->
      let f lhs rhs = T.(Bin (Mul, lhs, rhs)) in
      fold f 3 tokens
  | _ ->
      let f lhs rhs = T.(App (lhs, rhs)) in
      fold f 5 rest

let parse_expression = pratt 0

let parse_program tokens =
  let open Option.Let_syntax in
  let%bind { result ; rest } = parse_expression tokens in
  let%bind _ = consume EOF rest in
  return result
