open Core
open Prelude

module S = TAC (* source *)
module T = Cmm (* target *)

type 'a table = (STLC.ty, 'a, STLC.Ty.comparator_witness) Map.t

(* compute list of types in a list of instructions *)
let block_types =
  let filter = function
    | S.Store (_, t, _) -> Some t
    | S.Return _ -> None in
  List.filter_map ~f:filter

(*
let program_types S.{ functions ; body } =
  let definitions = List.map (Map.to_alist functions) ~f:snd in
  let bodies = List.map definitions ~f:(fun d -> d.body) in
  List.concat (List.map bodies ~f:block_types) @ block_types body

let index_map types =
  let sorted = List.dedup_and_sort types ~compare:STLC.Ty.compare in
  let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
  Map.of_alist_exn (module STLC.Ty) indexed
*)

let compile_program source =

  let function_bindings = Map.to_alist source.S.functions in
  let function_values = List.map function_bindings ~f:snd in

  let program_types =
    let bodies = List.map function_values ~f:(fun v -> v.body) in
    List.concat (List.map (source.S.body :: bodies) ~f:block_types) in

  let index_map =
    let sorted = List.dedup_and_sort program_types ~compare:STLC.Ty.compare in
    let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
    Map.of_alist_exn (module STLC.Ty) indexed in

  let type_name index = sprintf "T%d" index in
  let env_name index = sprintf "F%d" index in

  let atomic_type t =
    match t with
    | STLC.Z64 -> T.Z64
    | STLC.Arrow (domain, codomain) ->
        T.TypeSymbol (type_name (Map.find_exn index_map t)) in

  let environment S.{ env ; arg ; body = _ ; return_type = _ } =
    let f { name ; value } = { name ; value = atomic_type value } in
    T.Structure (List.map (arg :: env) ~f:f) in

  let environments =
    let f (k, v) = (env_name k, environment v) in
    let kvs = List.map function_bindings ~f:f in
    Map.of_alist_exn (module String) kvs in

  T.{
    types = environments ;
    procedures = Map.empty (module String) ;
    main = [] ;
  }
