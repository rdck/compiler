[@@@warning "+26"]
[@@@warning "+27"]
[@@@warning "+32"]
[@@@warning "+39"]
open Core
module S = STLC   (* source *)
module T = ALC    (* target *)

type 'a binding = 'a S.binding
[@@deriving equal, show]

type 'a environment = 'a binding list
[@@deriving equal, show]

let lookup (gamma : S.ty environment) (id : S.identifier) =
  let open S in
  let predicate binding = String.(=) id binding.name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

let rec synthesize gamma expression =
  let open S in
  let open Option.Let_syntax in
  let annotate x t = { T.expr = x ; T.note = t } in
  let return x t = Option.return (annotate x t) in
  match expression with
  | Lit i -> return (Lit i) Int
  | Bin (op, lhs, rhs) ->
      let%bind { expr = _ ; note = lht } as lhs' = synthesize gamma lhs in
      let%bind { expr = _ ; note = rht } as rhs' = synthesize gamma rhs in
      begin match (lht, rht) with
      | (Int, Int) -> return (Bin (op, lhs', rhs')) Int
      | _ -> None
      end
  | Var id -> Option.map (lookup gamma id) ~f:(annotate (Var id))
  | App (f, x) ->
      let%bind { expr = _ ; note = ft } as f' = synthesize gamma f in
      let%bind { expr = _ ; note = xt } as x' = synthesize gamma x in
      begin match (ft, xt) with
      | (Arrow (dom, cod), dom') when [%equal: ty] dom dom' ->
          return (App (f', x')) cod
      | _ -> None
      end
  | Abs ({ name ; value = dom } as binding, body) ->
      let%bind { expr = _ ; note = cod } as body' = synthesize (binding :: gamma) body in
      return (Abs (name, body')) (Arrow (dom, cod))

let rec forget { T.expr = expr ; T.note = note } =
  match expr with
  | T.Lit i -> S.Lit i
  | T.Bin (op, lhs, rhs) -> S.Bin (op, forget lhs, forget rhs)
  | T.Var id -> S.Var id
  | T.App (f, x) -> S.App (forget f, forget x)
  | T.Abs (id, body) ->
      begin match note with
      | S.Arrow (dom, _) -> S.Abs ({S.name = id ; S.value = dom}, forget body)
      | _ -> failwith "expected arrow type"
      end
