open Core

module S = ALC
module T = LLC

let rec free_vars gamma { S.expr ; S.note = _ } =
  match expr with
  | S.Lit _ -> []
  | S.Bin (_, lhs, rhs) -> free_vars gamma lhs @ free_vars gamma rhs
  | S.Var id ->
      let member = List.mem gamma id ~equal:[%equal: S.identifier] in
      if member then [] else [id]
  | S.App (f, x) -> free_vars gamma f @ free_vars gamma x
  | S.Abs (id, body) ->
      free_vars (id :: gamma) body

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

  let rec process gamma { S.expr ; S.note } =
    match expr with
    | S.Lit i ->
        {
          T.functions = empty ;
          T.body = {
            T.expr = T.Lit i ;
            T.note = note ;
          }
        }
    | S.Bin (op, lhs, rhs) ->
        let { T.functions = lhsf ; T.body = lhse } = process gamma lhs in
        let { T.functions = rhsf ; T.body = rhse } = process gamma rhs in
        {
          T.functions = Map.merge_disjoint_exn lhsf rhsf ;
          T.body = {
            T.expr = T.Bin (op, lhse, rhse) ;
            T.note = note ;
          }
        }
    | S.Var id ->
        {
          T.functions = empty ;
          T.body = {
            T.expr = T.Var id ;
            T.note = note ;
          }
        }
    | S.App (f, x) ->
        let { T.functions = ff ; T.body = fe } = process gamma f in
        let { T.functions = xf ; T.body = xe } = process gamma x in
        {
          T.functions = Map.merge_disjoint_exn ff xf ;
          T.body = {
            T.expr = T.App (fe, xe) ;
            T.note = note ;
          }
        }
    | S.Abs (name, body) ->
        let binding = STLC.{ name ; value = STLC.project_domain_exn note } in
        let fvs = free_vars [name] body in
        let bind id = STLC.{
          name = id ;
          value = lookup_exn gamma id ;
        } in
        let variable id = T.{
          expr = Var id ;
          note = lookup_exn gamma id
        } in
        let T.{ functions = bf ; body = be } = process (binding::gamma) body in
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

  in process [] term
