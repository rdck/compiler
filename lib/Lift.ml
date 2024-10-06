(******************************************************************************)
(* ANNOTATED -> LIFTED *)
(******************************************************************************)

open Core
open Prelude

module S = Annotated
module T = Lifted

let rec free_vars gamma S.{ expr ; note = _ } =
  let multi = match expr with
  | S.Lit _ -> []
  | S.Bin (_, lhs, rhs) -> free_vars gamma lhs @ free_vars gamma rhs
  | S.Var id ->
      let member = List.mem gamma id ~equal:[%equal: S.identifier] in
      if member then [] else [id]
  | S.App (f, x) -> free_vars gamma f @ free_vars gamma x
  | S.Abs (id, body) ->
      free_vars (id :: gamma) body in
  List.stable_dedup multi ~compare:String.compare

(* factor out *)
let lookup gamma id =
  let open STLC in
  let predicate binding = String.(=) id binding.name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

let lookup_exn gamma id =
  Option.value_exn (lookup gamma id)

let lift term =

  let counter = ref 0 in
  let gensym () =
    let out = !counter in
    counter := out + 1 ;
    out in
  let empty = Map.empty (module Int) in

  let rec process arg gamma S.{ expr ; note } =
    match expr with
    | S.Lit i ->
        T.{
          functions = empty ;
          body = {
            expr = Lit i ;
            note = note ;
          }
        }
    | S.Bin (op, lhs, rhs) ->
        let T.{ functions = lhsf ; body = lhse } = process arg gamma lhs in
        let T.{ functions = rhsf ; body = rhse } = process arg gamma rhs in
        T.{
          functions = Map.merge_disjoint_exn lhsf rhsf ;
          body = {
            expr = Bin (op, lhse, rhse) ;
            note = note ;
          }
        }
    | S.Var id ->
        T.{
          functions = empty ;
          body = {
            expr = if String.equal id arg then Var Arg else Var (Env id) ;
            note = note ;
          }
        }
    | S.App (f, x) ->
        let T.{ functions = ff ; body = fe } = process arg gamma f in
        let T.{ functions = xf ; body = xe } = process arg gamma x in
        T.{
          functions = Map.merge_disjoint_exn ff xf ;
          body = {
            expr = App (fe, xe) ;
            note = note ;
          }
        }
    | S.Abs (name, body) ->
        let binding = { name ; value = STLC.project_domain_exn note } in
        let fvs = free_vars [name] body in
        let bind id = STLC.{
          name = id ;
          value = lookup_exn gamma id ;
        } in
        let variable id = T.{
          expr = Var (Env id) ;
          note = lookup_exn gamma id
        } in
        let T.{ functions = bf ; body = be } = process name (binding::gamma) body in
        let def = T.{
          env = List.map fvs ~f:bind ;
          arg = binding ;
          body = be ;
        } in
        let sym = gensym () in
        T.{
          functions = Map.set bf ~key:sym ~data:def ;
          body = {
            expr = Closure (sym, List.map fvs ~f:variable) ;
            note = note ;
          }
        }

  in process "" [] term
