(******************************************************************************)
(* STLC -> ANNOTATED *)
(******************************************************************************)

open Core
open Prelude
open Types

module S = STLC       (* source *)
module T = Annotated  (* target *)

type 'a environment = (S.identifier, 'a) binding list

(* factor out *)
let lookup (gamma : ty environment) (id : S.identifier) =
  let predicate { name ; value = _ } = String.(=) id name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

let rec synthesize gamma expression =
  let open S in
  let open Option.Let_syntax in
  let annotate x t = { T.expr = x ; T.note = t } in
  let return x t = Option.return (annotate x t) in
  match expression with
  | Lit i -> return (Lit i) z64
  | Bin (op, lhs, rhs) ->
      let%bind { expr = _ ; note = lht } as lhs' = synthesize gamma lhs in
      let%bind { expr = _ ; note = rht } as rhs' = synthesize gamma rhs in
      begin match (lht, rht) with
      | (TypeSymbol lhs, TypeSymbol rhs)
      when String.equal lhs z64_symbol && String.equal rhs z64_symbol ->
        return (Bin (op, lhs', rhs')) z64
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
      | Arrow (dom, _) -> S.Abs ({name = id ; value = dom}, forget_exn body)
      | _ -> failwith "expected arrow type"
      end


let annotate program =

  let constructor_map { name = type_name ; value } =
    let f { name = constructor_name ; parameter } = (constructor_name, type_name) in
    Map.of_alist_exn (module String) (List.map value ~f) in

  let constructor_maps =
    List.map program.S.types ~f:constructor_map in

  let global_map =
    let empty = Map.empty (module String) in
    List.fold constructor_maps ~init:empty ~f:Map.merge_disjoint_exn in

  failwith ""

(*
let annotate = synthesize []
*)

let annotate_exn = Fn.compose value_exn annotate
