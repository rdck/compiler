open Core

module S = STLC
module T = LLC

let rec free_vars gamma = function
  | S.Lit _ -> []
  | S.Bin (_, lhs, rhs) -> free_vars gamma lhs @ free_vars gamma rhs
  | S.Var id ->
      let member = List.mem gamma id ~equal:[%equal: S.identifier] in
      if member then [] else [id]
  | S.App (f, x) -> free_vars gamma f @ free_vars gamma x
  | S.Abs ({ S.name ; _ }, body) ->
      free_vars (name :: gamma) body

(* factor out *)
let lookup gamma id =
  let open S in
  let predicate binding = String.(=) id binding.name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

let lookup_exn gamma id =
  Option.value_exn (lookup gamma id)

let lift expr =

  let counter = ref 0 in
  let gensym () =
    let out = !counter in
    counter := out + 1 ;
    out in
  let empty = Map.empty (module Int) in
  let return definitions body = { T.functions = definitions ; T.body = body } in

  let rec process gamma = function
    | S.Lit i ->
        return empty (T.Lit i)
    | S.Bin (op, lhs, rhs) ->
        let { T.functions = lhsf ; T.body = lhse } = process gamma lhs in
        let { T.functions = rhsf ; T.body = rhse } = process gamma rhs in
        return (Map.merge_disjoint_exn lhsf rhsf) (T.Bin (op, lhse, rhse))
    | S.Var id ->
        return empty (T.Var id)
    | S.App (f, x) ->
        let { T.functions = ff ; T.body = fe } = process gamma f in
        let { T.functions = xf ; T.body = xe } = process gamma x in
        return (Map.merge_disjoint_exn ff xf) (T.App (fe, xe))
    | S.Abs ({ S.name ; _ } as binding, body) ->
        let fvs = free_vars [name] body in
        let annotate id = { S.name = id ; S.value = lookup_exn gamma id } in
        let variable id = T.Var id in
        let { T.functions = bf ; T.body = be } = process (binding::gamma) body in
        let def = {
          T.env = List.map fvs ~f:annotate ;
          T.arg = binding ;
          T.body = be ;
          T.return_type = failwith "TODO" ;
        } in
        let sym = gensym () in {
          T.functions = Map.set bf ~key:sym ~data:def ;
          T.body = T.Closure (sym, List.map fvs ~f:variable) ;
        } in

  process [] expr
