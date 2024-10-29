(******************************************************************************)
(* Syntax -> ANNOTATED *)
(******************************************************************************)

open Core
open Prelude
open Types
open Result.Let_syntax

module S = Syntax       (* source *)
module T = Elaboration  (* target *)

type 'a environment = (S.identifier, 'a) bindings

let fail = Result.fail

(* factor out *)
let lookup (gamma : ty environment) (id : S.identifier) =
  let predicate { name ; value = _ } = String.(=) id name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

type constructor_spec = {
  family : string ;
  parameter : ty ;
}

let elaborate_program program =

  (* constructor symbol table *)
  let constructor_table =

    (* build map from a single type definition *)
    let constructor_map { name = family ; value } =
      let f { name = constructor ; parameter } =
        (constructor, { family ; parameter }) in
      Map.of_alist_exn (module String) (List.map value ~f) in

    (* total list of maps *)
    let constructor_maps = List.map program.S.types ~f:constructor_map in

    (* empty map *)
    let empty = Map.empty (module String) in

    (* fold maps *)
    List.fold constructor_maps ~init:empty ~f:Map.merge_disjoint_exn in

  (* constructor lookup function *)
  let lookup_constructor = Map.find constructor_table in
  let lookup_constructor_exn = Map.find_exn constructor_table in

  (* elaborate an expression in a typing context *)
  let rec synth gamma expression =

    let open S in
    let output x t = return (T.expression x t) in

    match expression with

    | Lit i -> output (Lit i) z64

    | Bin (op, lhs, rhs) ->
        let%bind { expr = _ ; note = lht } as lhe = synth gamma lhs in
        let%bind { expr = _ ; note = rht } as rhe = synth gamma rhs in
        begin match (lht, rht) with
        | (TypeSymbol lhs, TypeSymbol rhs)
        when String.equal lhs z64_symbol && String.equal rhs z64_symbol ->
          output (Bin (op, lhe, rhe)) z64
        | _ -> fail "expected integer type for binary operator"
        end

    | Var id ->
        Result.of_option ~error:(sprintf "unbound variable %s" id) (
          Option.map (lookup gamma id) ~f:(T.expression (Var id))
        )

    | App (f, x) ->
        let%bind { expr = _ ; note = ft } as fe = synth gamma f in
        let%bind { expr = _ ; note = xt } as xe = synth gamma x in
        begin match (ft, xt) with
        | (Arrow (dom, cod), dom') when [%equal: ty] dom dom' ->
            output (App (fe, xe)) cod
        | _ -> fail "expected arrow type"
        end

    | Abs ({ name ; value = dom } as binding, body) ->
        let%bind { expr = _ ; note = cod } as body = synth (binding :: gamma) body in
        output (Abs (name, body)) (Arrow (dom, cod))

    | Con (c, p) ->
        let%bind { family ; parameter = expect } =
          let error_message = sprintf "unknown constructor: %s" c in
          Result.of_option (lookup_constructor c) ~error:error_message in
        let%bind actual = synth gamma p in
        if [%equal: ty] actual.T.note expect
        then output (Con (c, actual)) (TypeSymbol family)
        else fail "constructor parameter type mismatch"

    | Mat (control, cases) ->
        let%bind { expr = _ ; note = expect } as control = synth gamma control in
        let f ({ name ; parameter }, body) =
          let%bind spec =
            let error_message = sprintf "unknown constructor: %s" name in
            Result.of_option (lookup_constructor name) ~error:error_message in
          if [%equal: ty] (TypeSymbol spec.family) expect
          then
            let%bind body = synth (binding parameter spec.parameter :: gamma) body in
            return (T.{ name ; parameter ; parameter_type = spec.parameter }, body)
          else fail "unexpected family" in
        let%bind annotated_cases = Result.all (List.map cases ~f) in
        let annotations = List.map annotated_cases ~f:(fun (_, c) -> c.note) in
        let%bind body_type = Result.of_option ~error:"pattern match bodies don't match" (
          List.all_equal annotations ~equal:[%equal: ty]
        ) in
        output (Mat (control, annotated_cases)) body_type in

  let%bind body = synth [] program.S.body in

  return T.{
    types = program.S.types ;
    body = body ;
  }
