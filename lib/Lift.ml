(******************************************************************************)
(* ANNOTATED -> LIFTED *)
(******************************************************************************)

open Core
open Symbol
open Types
module S = Elaboration (* source *)
module T = Apex (* target *)

let free_vars expr =
  let filter id name = not (String.equal id name) in
  let rec multi S.{ expr; note = _ } =
    match expr with
    | S.Lit _ -> []
    | S.Bin (_, lhs, rhs) -> multi lhs @ multi rhs
    | S.Var id -> [ id ]
    | S.App (f, x) -> multi f @ multi x
    | S.Abs (id, body) -> List.filter (multi body) ~f:(filter id)
    | S.Con (_, p) -> multi p
    | S.Mat (control, cases) ->
      let cases_vars =
        List.map cases ~f:(fun (pattern, body) ->
          List.filter (multi body) ~f:(filter pattern.parameter))
      in
      let control_vars = multi control in
      List.concat (control_vars :: cases_vars)
    | S.Rec _ -> failwith "TODO"
  in
  List.stable_dedup (multi expr) ~compare:String.compare


(* factor out *)
let lookup (gamma : (identifier, ty) bindings) (id : identifier) =
  let predicate binding = String.equal id binding.Symbol.name in
  Option.map (List.find gamma ~f:predicate) ~f:binding_value


let lookup_exn gamma id = Option.value_exn (lookup gamma id)

type lift =
  { terms : (symbol, T.definition) bindings
  ; body : T.term
  }

let lift_program S.{ types; body } =
  (* initialize local symbol generator *)
  let counter = ref 0 in
  let gensym _ =
    let index = !counter in
    counter := index + 1;
    index
  in
  let rec lift gamma (S.{ expr; note } as node) =
    let translate expr = T.{ expr; note } in
    let output terms body = { terms; body = translate body } in
    let var id =
      match List.hd gamma with
      | Some { name; value = _ } -> if String.equal name id then T.Arg id else T.Var id
      | None -> Var id
    in
    match expr with
    | S.Lit i -> output [] (Lit i)
    | S.Bin (op, lhs, rhs) ->
      let { terms = lhs_terms; body = lhs_body } = lift gamma lhs in
      let { terms = rhs_terms; body = rhs_body } = lift gamma rhs in
      output (lhs_terms @ rhs_terms) (Bin (op, lhs_body, rhs_body))
    | S.Var id -> output [] (var id)
    | S.App (lhs, rhs) ->
      let { terms = lhs_terms; body = lhs_body } = lift gamma lhs in
      let { terms = rhs_terms; body = rhs_body } = lift gamma rhs in
      output (lhs_terms @ rhs_terms) (App (lhs_body, rhs_body))
    | S.Abs (id, body) ->
      let fvs = free_vars node in
      let symbol = gensym "main" in
      let { terms = body_terms; body = body_body } =
        let argument = binding id (ty_domain_exn note) in
        lift (argument :: gamma) body
      in
      { terms =
          (let definition =
             T.
               { env = List.map fvs ~f:(fun v -> binding v (lookup_exn gamma v))
               ; arg = binding id (ty_domain_exn note)
               ; body = body_body
               }
           in
           binding symbol definition :: body_terms)
      ; body =
          (let args =
             List.map fvs ~f:(fun v -> T.{ expr = var v; note = lookup_exn gamma v })
           in
           translate (Cls (symbol, args)))
      }
    | S.Con (c, p) ->
      let { terms; body } = lift gamma p in
      output terms (Con (c, body))
    | S.Mat (control, cases) ->
      (* TODO: shared with abs *)
      let var id =
        match List.hd gamma with
        | Some { name; value = t } ->
          let x = if String.equal name id then T.Arg id else T.Var id in
          T.annotate x t
        | None -> T.annotate (Var id) (lookup_exn gamma id)
      in
      let fvs = free_vars node in
      let { terms = control_terms; body = control_body } = lift gamma control in
      (* lift a case *)
      let f (pattern, body) =
        (* lift body *)
        let { terms = body_terms; body = body_body } =
          let argument = binding pattern.S.parameter pattern.S.parameter_type in
          lift (argument :: gamma) body
        in
        let symbol = gensym "match" in
        let definition =
          T.
            { env = List.map fvs ~f:(fun v -> binding v (lookup_exn gamma v))
            ; arg = binding pattern.S.parameter pattern.S.parameter_type
            ; body = body_body
            }
        in
        let closure_type = Arrow (pattern.parameter_type, body.note) in
        let closure =
          T.annotate (T.Cls (symbol, List.map fvs ~f:(fun v -> var v))) closure_type
        in
        let argument = T.annotate (T.Arg pattern.S.parameter) pattern.S.parameter_type in
        { terms = binding symbol definition :: body_terms
        ; body = T.annotate (T.App (closure, argument)) body.S.note
        }
      in
      let cases = List.map cases ~f in
      let cases_terms = List.map cases ~f:(fun c -> c.terms) in
      let cases_bodies = List.map cases ~f:(fun c -> c.body) in
      let symbols =
        List.map cases_bodies ~f:(fun expr ->
          match expr.expr with
          | T.(App (f, _)) ->
            (match f.expr with
             | Cls (symbol, _) -> symbol
             | _ -> failwith "ill formed match compilation")
          | _ -> failwith "ill formed match compilation")
      in
      let body = T.Mat (control_body, List.map fvs ~f:(fun v -> var v), symbols) in
      output (control_terms @ List.concat cases_terms) body
    | S.Rec _ -> failwith "TODO"
  in
  let { terms; body } = lift [] body in
  T.{ types; terms; body }
