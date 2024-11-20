(******************************************************************************)
(* ANNOTATED -> LIFTED *)
(******************************************************************************)

open Core
open Symbol
open Types
module S = Elaboration (* source *)
module T = Apex (* target *)

let filter id name = not (String.equal id name)

(* find the free variables in an expression *)
let free expr =
  (* compute the free variables without removing duplicates *)
  let rec multi S.{ expr; note = _ } =
    match expr with
    | S.Lit _ -> []
    | S.Bin (_, lhs, rhs) -> multi lhs @ multi rhs
    | S.Var id -> [ id ]
    | S.App (f, x) -> multi f @ multi x
    | S.Abs (id, body) -> List.filter (multi body) ~f:(filter id)
    | S.Con (_, p) -> multi p
    | S.Mat (control, cases) ->
      (* union of free variables in each case *)
      let free_in_cases =
        (* find the free variables in a case *)
        let free_in_case (pattern, body) =
          List.filter (multi body) ~f:(filter pattern.S.parameter)
        in
        List.map cases ~f:free_in_case
      in
      List.concat (multi control :: free_in_cases)
    | S.Let (id, e, b) ->
      let free_in_definition = multi e in
      let free_in_body = List.filter (multi b) ~f:(filter id) in
      free_in_definition @ free_in_body
    | S.Conditional (antecedent, consequent, alternative) ->
      multi antecedent @ multi consequent @ multi alternative
  in
  List.stable_dedup (multi expr) ~compare:String.compare


(* The result of lifting a term is a set of lifted definitions together with the modified
   term. *)
type 'a lift =
  { terms : (symbol, T.definition) bindings
  ; body : 'a
  }

let lift_terms lift = lift.terms
let lift_body lift = lift.body

let lift_program S.{ types; body } =
  (* initialize local symbol generator *)
  let counter = ref 0 in
  let gensym _ =
    let index = !counter in
    counter := index + 1;
    index
  in
  let rec lift gamma (S.{ expr; note } as node) =
    (* annotate an expression with the type of the input term *)
    let annotate expr = T.{ expr; note } in
    (* curried constructor for the result of a lift *)
    let output terms body = { terms; body = annotate body } in
    let var id = T.annotate (Var id) (lookup_exn gamma id) in
    match expr with
    | S.Lit i -> output [] (Lit i)
    | S.Bin (op, lhs, rhs) ->
      let { terms = lhs_terms; body = lhs_body } = lift gamma lhs in
      let { terms = rhs_terms; body = rhs_body } = lift gamma rhs in
      output (lhs_terms @ rhs_terms) (Bin (op, lhs_body, rhs_body))
    | S.Var id -> output [] (Var id)
    | S.App (lhs, rhs) ->
      let { terms = lhs_terms; body = lhs_body } = lift gamma lhs in
      let { terms = rhs_terms; body = rhs_body } = lift gamma rhs in
      output (lhs_terms @ rhs_terms) (App (lhs_body, rhs_body))
    | S.Abs (id, body) ->
      let fvs = free node in
      let symbol = gensym "main" in
      let { terms = body_terms; body = body_body } =
        let argument = binding id (ty_domain_exn note) in
        lift (argument :: gamma) body
      in
      { terms =
          begin
            let definition =
              T.
                { env = List.map fvs ~f:(fun v -> binding v (lookup_exn gamma v))
                ; arg = binding id (ty_domain_exn note)
                ; body = body_body
                }
            in
            binding symbol definition :: body_terms
          end
      ; body = annotate (Cls { code = symbol; data = List.map fvs ~f:var })
      }
    | S.Con (c, p) ->
      let { terms; body } = lift gamma p in
      output terms (Con (c, body))
    | S.Mat (control, cases) ->
      (* lift the control term *)
      let { terms = control_terms; body = control_body } = lift gamma control in
      (* lift each case *)
      let cases =
        let lift_case (pattern, body) =
          (* lift the body of the case *)
          let { terms = case_terms; body = case_body } =
            let argument = binding pattern.S.parameter pattern.S.parameter_type in
            lift (argument :: gamma) body
          in
          (* generate a symbol *)
          let symbol = gensym "match" in
          (* compute the free variables in the case body *)
          let free_variables = List.filter (free body) ~f:(filter pattern.S.parameter) in
          (* build the lifted function definition *)
          let definition =
            T.
              { env = List.map free_variables ~f:(fun v -> binding v (lookup_exn gamma v))
              ; arg = binding pattern.S.parameter pattern.S.parameter_type
              ; body = case_body
              }
          in
          { terms = binding symbol definition :: case_terms
          ; body = T.closure symbol (List.map free_variables ~f:var)
          }
        in
        List.map cases ~f:lift_case
      in
      let cases_terms = List.concat_map cases ~f:lift_terms in
      let cases_bodies = List.map cases ~f:lift_body in
      output (control_terms @ cases_terms) (Mat (control_body, cases_bodies))
    | S.Let (id, e, b) ->
      let { terms = et; body = eb } = lift gamma e in
      let { terms = bt; body = bb } = lift (binding id e.note :: gamma) b in
      output (et @ bt) (T.Let (id, eb, bb))
    | S.Conditional (antecedent, consequent, alternative) ->
      let { terms = antecedent_terms; body = antecedent_body } = lift gamma antecedent in
      let { terms = consequent_terms; body = consequent_body } = lift gamma consequent in
      let { terms = alternative_terms; body = alternative_body } =
        lift gamma alternative
      in
      output
        (antecedent_terms @ consequent_terms @ alternative_terms)
        T.(Conditional (antecedent_body, consequent_body, alternative_body))
  in
  let { terms; body } = lift [] body in
  T.{ types; terms; body }
