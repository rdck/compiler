(******************************************************************************)
(* STLC -> ANNOTATED *)
(******************************************************************************)

open Core
open Prelude
open Types

module S = STLC       (* source *)
module T = Annotated  (* target *)

type 'a environment = (S.identifier, 'a) bindings

(* factor out *)
let lookup (gamma : ty environment) (id : S.identifier) =
  let predicate { name ; value = _ } = String.(=) id name in
  let projection binding = binding.value in
  Option.map (List.find gamma ~f:predicate) ~f:projection

let forget_pattern T.{ name ; parameter ; parameter_type } =
  S.{ name ; parameter }

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
  | T.Con (id, p) -> S.Con (id, forget_exn p)
  | T.Mat (control, cases) ->
      let f (pattern, body) = (forget_pattern pattern, forget_exn body) in
      S.Mat (forget_exn control, List.map cases ~f)

type constructor_spec = {
  family : string ;
  parameter : ty ;
}

let all xs =
  let rec f acc = function
    | [] -> Some acc
    | Some x :: xs -> (f [@tailcall]) (x :: acc) xs
    | None :: _ -> None in
  Option.map ~f:List.rev (f [] xs)

let rec fold_option f z =
  let open Option.Let_syntax in function
    | [] -> return []
    | x :: xs ->
        let%bind z = f z x in
        (fold_option [@tailcall]) f z xs

let annotate program =

  let open Option.Let_syntax in

  (* constructor symbol table *)
  let constructor_table =

    (* build map from a single type definition *)
    let constructor_map { name = family ; value } =
      let f { name = constructor ; parameter } =
        (constructor, { family ; parameter }) in
      Map.of_alist_exn (module String) (List.map value ~f) in

    (* total list of maps *)
    let constructor_maps =
      List.map program.S.types ~f:constructor_map in

    let empty = Map.empty (module String) in

    (* fold maps *)
    List.fold constructor_maps ~init:empty ~f:Map.merge_disjoint_exn in

  (* constructor lookup function *)
  let lookup_constructor = Map.find_exn constructor_table in

  (* elaborate an expression in a typing context *)
  let rec synth gamma expression =
    let open S in
    let annotate x t = { T.expr = x ; T.note = t } in
    let return x t = Option.return (annotate x t) in
    match expression with
    | Lit i -> return (Lit i) z64
    | Bin (op, lhs, rhs) ->
        let%bind { expr = _ ; note = lht } as lhs' = synth gamma lhs in
        let%bind { expr = _ ; note = rht } as rhs' = synth gamma rhs in
        begin match (lht, rht) with
        | (TypeSymbol lhs, TypeSymbol rhs)
        when String.equal lhs z64_symbol && String.equal rhs z64_symbol ->
          return (Bin (op, lhs', rhs')) z64
        | _ -> None
        end
    | Var id -> Option.map (lookup gamma id) ~f:(annotate (Var id))
    | App (f, x) ->
        let%bind { expr = _ ; note = ft } as f' = synth gamma f in
        let%bind { expr = _ ; note = xt } as x' = synth gamma x in
        begin match (ft, xt) with
        | (Arrow (dom, cod), dom') when [%equal: ty] dom dom' ->
            return (App (f', x')) cod
        | _ -> None
        end
    | Abs ({ name ; value = dom } as binding, body) ->
        let%bind { expr = _ ; note = cod } as body' = synth (binding :: gamma) body in
        return (Abs (name, body')) (Arrow (dom, cod))
    | Con (c, p) ->
        let { family ; parameter = expect } = lookup_constructor c in
        let%bind actual = synth gamma p in
        if [%equal: ty] actual.T.note expect
        then return (Con (c, actual)) (TypeSymbol family)
        else None
    | Mat (control, cases) ->
        let%bind { expr = _ ; note = expect } as control = synth gamma control in
        let f ({ name ; parameter }, body) =
          let spec = lookup_constructor name in
          if [%equal: ty] (TypeSymbol spec.family) expect
          then
            let%bind body = synth (binding parameter spec.parameter :: gamma) body in
            Some (T.{ name ; parameter ; parameter_type = spec.parameter }, body)
          else None in
        let%bind annotated_cases = all (List.map cases ~f) in
        let annotations = List.map annotated_cases ~f:(fun (_, c) -> c.note) in
        let%bind body_type = List.all_equal annotations ~equal:[%equal: ty] in
        return (Mat (control, annotated_cases)) body_type in

  let%bind body = synth [] program.S.body in

  Some T.{
    types = program.S.types ;
    body = body ;
  }

let annotate_exn prog = Option.value_exn (annotate prog)
