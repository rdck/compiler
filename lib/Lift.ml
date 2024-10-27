(******************************************************************************)
(* ANNOTATED -> LIFTED *)
(******************************************************************************)

open Core
open Prelude
open Types

module S = Elaboration  (* source *)
module T = Apex         (* target *)

let free_vars expr =

  let filter id = fun name -> not (String.equal id name) in

  let rec multi S.{ expr ; note = _ } =
    match expr with
    | S.Lit _ -> []
    | S.Bin (_, lhs, rhs) -> multi lhs @ multi rhs
    | S.Var id -> [id]
    | S.App (f, x) -> multi f @ multi x
    | S.Abs (id, body) ->
        List.filter (multi body) ~f:(filter id)
    | S.Con (c, p) -> multi p
    | S.Mat (control, cases) ->
        let cases_vars = List.map cases ~f:(fun (pattern, body) ->
          List.filter (multi body) ~f:(filter pattern.parameter)
        ) in
        let control_vars = multi control in
        List.concat (control_vars :: cases_vars) in

  List.stable_dedup (multi expr) ~compare:String.compare

(* factor out *)
let lookup (gamma : (identifier, ty) bindings) (id : identifier) =
  let predicate binding = String.equal id binding.Prelude.name in
  Option.map (List.find gamma ~f:predicate) ~f:project_value

let lookup_exn gamma id =
  Option.value_exn (lookup gamma id)

type lift = {
  terms : (T.symbol, T.definition) bindings ;
  body : T.term ;
}

let lift_program S.{ types ; body } =

  (* initialize local symbol generator *)
  let counter = ref 0 in
  let gensym name =
    let index = !counter in
    counter := index + 1 ;
    index in

  let rec lift gamma (S.{ expr ; note } as node) =

    let translate expr = T.{ expr ; note } in
    let output terms body = { terms ; body = translate body } in
    let var id = match List.hd gamma with
      | Some { name ; value = _ } ->
          if String.equal name id then T.Arg id else T.Var id
      | None -> Var id in

    match expr with
    | S.Lit i -> output [] (Lit i)
    | S.Bin (op, lhs, rhs) ->
        let { terms = lhs_terms ; body = lhs_body } = lift gamma lhs in
        let { terms = rhs_terms ; body = rhs_body } = lift gamma rhs in
        output (lhs_terms @ rhs_terms) (Bin (op, lhs_body, rhs_body))
    | S.Var id -> output [] (var id)
    | S.App (lhs, rhs) ->
        let { terms = lhs_terms ; body = lhs_body } = lift gamma lhs in
        let { terms = rhs_terms ; body = rhs_body } = lift gamma rhs in
        output (lhs_terms @ rhs_terms) (App (lhs_body, rhs_body))
    | S.Abs (id, body) ->
        let fvs = free_vars node in
        let symbol = gensym "main" in
        let { terms = body_terms ; body = body_body } =
          let argument = binding id (project_domain_exn note) in
          lift (argument :: gamma) body in
        {
          terms = begin
            let definition = T.{
              env = List.map fvs ~f:(fun v -> binding v (lookup_exn gamma v)) ;
              arg = binding id (project_domain_exn note) ;
              body = body_body ;
            } in (binding symbol definition) :: body_terms
          end ;
          body = begin
            let args = List.map fvs ~f:(fun v ->
              T.{ expr = var v ; note = lookup_exn gamma v }
            ) in
            translate (Cls (symbol, args))
          end ;
        }
    | S.Con (c, p) ->
        let { terms ; body } = lift gamma p in
        output terms (Con (c, body))
    | S.Mat (control, cases) ->
        let { terms = control_terms ; body = control_body } = lift gamma control in
        let (cases_terms, cases) = List.fold_map cases ~init:[] ~f:(
          fun acc (pattern, case) ->
            let { terms ; body } = lift gamma case in
            (acc @ terms, (pattern, body))
        ) in
        output (control_terms @ cases_terms) (Mat (control_body, cases)) in

  let { terms ; body } = lift [] body in

  T.{
    types = types ;
    terms = terms ;
    body = body ;
  }
