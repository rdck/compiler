open Core
open Symbol
module S = Apex (* source *)
module T = Toponym (* target *)

(* the context keeps a list of local variables *)
let rec track_expression gamma argument expr =
  let track = track_expression in
  let annotate e = T.annotate e expr.S.note in
  let inner =
    match expr.expr with
    | S.Lit i -> T.Lit i
    | S.Bin (op, lhs, rhs) ->
      T.Bin (op, track gamma argument lhs, track gamma argument rhs)
    | S.Var id ->
      let namespace =
        match List.find gamma ~f:(String.equal id) with
        | Some _ -> T.Loc
        | None -> if String.equal id argument then T.Arg else T.Env
      in
      T.Var (namespace, id)
    | S.Cls { code; data } ->
      let data = List.map data ~f:(track gamma argument) in
      T.Cls { code; data }
    | S.App (f, x) -> T.App (track gamma argument f, track gamma argument x)
    | S.Con (c, p) -> T.Con (c, track gamma argument p)
    | S.Mat (control, cases) ->
      let control = track gamma argument control in
      let cases =
        let track_case S.{ code; data } =
          T.{ code; data = List.map data ~f:(track gamma argument) }
        in
        List.map cases ~f:track_case
      in
      T.Mat (control, cases)
    | S.Let (id, e, b) ->
      let extended = id :: gamma in
      T.Let (id, track gamma argument e, track extended argument b)
    | S.Conditional (antecedent, consequent, alternative) ->
      T.Conditional
        ( track gamma argument antecedent
        , track gamma argument consequent
        , track gamma argument alternative )
  in
  annotate inner


let track_definition binding =
  let d = binding.value in
  Symbol.binding
    binding.name
    T.{ env = d.S.env; arg = d.S.arg; body = track_expression [] d.S.arg.name d.S.body }


let track_program program =
  T.
    { types = program.S.types
    ; terms = List.map program.S.terms ~f:track_definition
    ; body = track_expression [] "" program.S.body
    }
