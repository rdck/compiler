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

let rec parse_type tokens =

  let open Option.Let_syntax in
  let ret result rest = return { result ; rest } in

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { result ; rest } = parse_type rest in
        let%bind rest = consume ShutParen rest in
        ret result rest
    | Z64 :: rest -> ret STLC.Z64 rest
    | _ -> None in

  let%bind { result = lhs ; rest } = parse_atom tokens in
  let rhs =
    let%bind rest = consume Arrow rest in
    parse_type rest in
  match rhs with
  | Some { result = rhs ; rest } -> ret (STLC.Arrow (lhs, rhs)) rest
  | None -> ret lhs rest

let rec parse_precedence p tokens =

  let open Option.Let_syntax in
  let ret result rest = return { result ; rest } in

  let parse_atom = function
    | OpenParen :: rest ->
        let%bind { result ; rest } = parse_precedence 0 rest in
        let%bind rest = consume ShutParen rest in
        ret result rest
    | Identifier id :: rest -> ret (T.Var id) rest
    | Literal l :: rest -> ret (T.Lit l) rest
    | _ -> None in

  (* function keyword *)
  match tokens with
  | Lambda :: Identifier id :: Colon :: rest ->
      let%bind { result = t ; rest } = parse_type rest in
      let%bind rest = consume Period rest in
      let%bind { result = e ; rest } = parse_precedence 1 rest in
      ret STLC.(Abs ({ name = id ; value = t }, e)) rest
  | _ ->
      let%bind { result = lhs ; rest } = parse_atom tokens in
      let { result ; rest } = loop p lhs rest in
      ret result rest

and loop p lhs rest =

  let open Option.Let_syntax in
  let default = { result = lhs ; rest } in

  match rest with
  | Plus :: rest' ->
      let p' = 2 in
      begin match p' > p with
      | true ->
          let looped =
            let%bind { result = rhs ; rest } = parse_precedence p' rest' in
            return @@ loop p (T.Bin (T.Add, lhs, rhs)) rest in
          Option.value looped ~default
      | false -> default
      end
  | Minus :: rest' ->
      let p' = 2 in
      begin match p' > p with
      | true ->
          let looped =
            let%bind { result = rhs ; rest } = parse_precedence p' rest' in
            return @@ loop p (T.Bin (T.Sub, lhs, rhs)) rest in
          Option.value looped ~default
      | false -> default
      end
  | Star :: rest' ->
      let p' = 3 in
      begin match p' > p with
      | true ->
          let looped =
            let%bind { result = rhs ; rest } = parse_precedence p' rest' in
            return @@ loop p (T.Bin (T.Mul, lhs, rhs)) rest in
          Option.value looped ~default
      | false -> default
      end
  | _ ->
      let p' = 5 in
      begin match p' > p with
      | true ->
          let looped =
            let%bind { result = rhs ; rest } = parse_precedence p' rest in
            return @@ loop p (T.App (lhs, rhs)) rest in
          Option.value looped ~default
      | false -> default
      end

let parse = parse_precedence 0

let parse_program tokens =
  let open Option.Let_syntax in
  let%bind { result ; rest } = parse tokens in
  let%bind _ = consume EOF rest in
  return result
