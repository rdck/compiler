open Core
open Token
module T = STLC (* target *)

type 't parse = {
  result : 't ;
  remaining : token list ;
}

let consume token = function
  | t :: ts -> if [%equal:token] t token then Some ts else None
  | _ -> None

let rec parse_type tokens =
  let open Option.Let_syntax in
  let ret result remaining = return { result ; remaining } in
  match tokens with
  | Z64 :: rest -> ret T.Z64 rest
  | OpenParen :: rest ->
      let%bind { result = domain ; remaining } = parse_type rest in
      let%bind rest = consume Arrow remaining in
      let%bind { result = codomain ; remaining } = parse_type rest in
      let%bind rest = consume ShutParen remaining in
      ret (STLC.Arrow (domain, codomain)) rest
  | _ -> None

let rec parse_expr tokens =
  let open Option.Let_syntax in
  let ret result remaining = return { result ; remaining } in
  match tokens with
  | Literal i :: rest -> ret (T.Lit i) rest
  | Identifier id :: rest -> ret (T.Var id) rest
  | OpenParen :: rest ->
      let%bind { result ; remaining } = parse_subexpr rest in
      let%bind rest = consume ShutParen remaining in
      ret result rest
  | _ -> None
and parse_subexpr tokens =
  let open Option.Let_syntax in
  let ret result remaining = return { result ; remaining } in
  match tokens with
  | Plus :: remaining ->
      let%bind { result = lhs ; remaining } = parse_expr remaining in
      let%bind { result = rhs ; remaining } = parse_expr remaining in
      ret (T.Bin (T.Add, lhs, rhs)) remaining
  | Minus :: remaining ->
      let%bind { result = lhs ; remaining } = parse_expr remaining in
      let%bind { result = rhs ; remaining } = parse_expr remaining in
      ret (T.Bin (T.Sub, lhs, rhs)) remaining
  | Star :: remaining ->
      let%bind { result = lhs ; remaining } = parse_expr remaining in
      let%bind { result = rhs ; remaining } = parse_expr remaining in
      ret (T.Bin (T.Mul, lhs, rhs)) remaining
  | Lambda :: Identifier id :: Colon :: remaining ->
      let%bind { result = t ; remaining } = parse_type remaining in
      let%bind remaining = consume Period remaining in
      let%bind { result = body ; remaining } = parse_expr remaining in
      ret (T.Abs ({ name = id ; value = t }, body)) remaining
  | remaining ->
      let%bind { result = f ; remaining } = parse_expr remaining in
      let%bind { result = x ; remaining } = parse_expr remaining in
      ret (T.App (f, x)) remaining

let parse_program tokens =
  let open Option.Let_syntax in
  let%bind { result ; remaining } = parse_expr tokens in
  let%bind _ = consume EOF remaining in
  return result
