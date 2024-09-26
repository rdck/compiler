(******************************************************************************)
(* STLC -> ANNOTATED *)
(******************************************************************************)

open Core
open Prelude

module S = STLC       (* source *)
module T = Annotated  (* target *)

type 'a environment = (S.identifier, 'a) binding list

(* factor out *)
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
  | Lit i -> return (Lit i) Z64
  | Bin (op, lhs, rhs) ->
      let%bind { expr = _ ; note = lht } as lhs' = synthesize gamma lhs in
      let%bind { expr = _ ; note = rht } as rhs' = synthesize gamma rhs in
      begin match (lht, rht) with
      | (Z64, Z64) -> return (Bin (op, lhs', rhs')) Z64
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

let rec forget_exn T.{ expr ; note } =
  match expr with
  | T.Lit i -> S.Lit i
  | T.Bin (op, lhs, rhs) -> S.Bin (op, forget_exn lhs, forget_exn rhs)
  | T.Var id -> S.Var id
  | T.App (f, x) -> S.App (forget_exn f, forget_exn x)
  | T.Abs (id, body) ->
      begin match note with
      | S.Arrow (dom, _) -> S.Abs ({name = id ; value = dom}, forget_exn body)
      | _ -> failwith "expected arrow type"
      end

let annotate = synthesize []

let annotate_exn = Fn.compose value_exn annotate
