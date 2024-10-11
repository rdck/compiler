open Core
open Token

module T = STLC (* target *)

type 't parse = {
  result : 't ;
  rest : token list ;
}

let consume token = function
  | t :: ts -> if [%equal:token] t token then Some ts else None
  | _ -> None

let rec parse_precedence p tokens =
  let open Option.Let_syntax in
  let open Continue_or_stop in
  let ret result rest = return { result ; rest } in
  let parse_atom tokens =
    match tokens with
    | OpenParen :: rest ->
        let%bind { result ; rest } = parse_precedence 0 rest in
        let%bind rest = consume ShutParen rest in
        ret result rest
    | Identifier id :: rest -> ret (T.Var id) rest
    | Literal l :: rest -> ret (T.Lit l) rest
    | _ -> None in
  let%bind { result = lhs ; rest } = parse_atom tokens in
  let { result ; rest } = loop p lhs rest in
  ret result rest
and loop p lhs rest =
  match rest with
  | Plus :: rest' ->
      let p' = 2 in
      begin match p' > p with
      | true ->
          begin match parse_precedence p' rest' with
          | Some { result = rhs ; rest } -> loop p (T.Bin (T.Add, lhs, rhs)) rest
          | None -> { result = lhs ; rest }
          end
      | false -> { result = lhs ; rest }
      end
  | Star :: rest' ->
      let p' = 3 in
      begin match p' > p with
      | true ->
          begin match parse_precedence p' rest' with
          | Some { result = rhs ; rest } -> loop p (T.Bin (T.Mul, lhs, rhs)) rest
          | None -> { result = lhs ; rest }
          end
      | false -> { result = lhs ; rest }
      end
  | _ -> { result = lhs ; rest }

let parse = parse_precedence 0
