open Core
open Prelude

module S = TAC (* source *)
module T = Cmm (* target *)

type 'a table = (STLC.ty, 'a, STLC.Ty.comparator_witness) Map.t

let compile_program source =

  (* compute list of types in a list of instructions *)
  let block_types =
    let filter = function
      | S.Store (_, t, _) -> Some t
      | S.Return _ -> None in
    List.filter_map ~f:filter in

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
  let type_tag_name index = sprintf "T%dTag" index in

  let atomic_type t =
    match t with
    | STLC.Z64 -> T.Z64
    | STLC.Arrow (domain, codomain) ->
        T.TypeSymbol (type_name (Map.find_exn index_map t)) in

  let environments =

    let environment S.{ env ; arg ; body = _ ; return_type = _ } =
      let f { name ; value } = { name ; value = atomic_type value } in
      T.Structure (List.map (arg :: env) ~f:f) in

    let f (k, v) = { name = env_name k ; value = environment v } in
    List.map function_bindings ~f:f in

  let types_to_functions =
    let folder ~key ~data acc =
      let t = STLC.Arrow (data.S.arg.value, data.return_type) in
      let t' = Map.find_exn index_map t in
      let existing_list = Map.find_multi acc t' in
      Map.set acc ~key:t' ~data:(key :: existing_list) in
    let empty = Map.empty (module Int) in
    Map.fold source.S.functions ~init:empty ~f:folder in

  let types_to_functions' =
    Map.to_alist types_to_functions in

  let lookup_type t =
    let k = Map.find_exn index_map t in
    Map.find_exn types_to_functions k in

  let function_tags =
    let to_binding (type_index, function_indices) =
      let item_name idx =
        sprintf "%s_%s" (type_name type_index) (env_name idx) in
      let ids = List.map function_indices ~f:item_name in
      { name = type_tag_name type_index ; value = T.Enumeration ids } in
    List.map types_to_functions' ~f:to_binding in

  let function_unions =
    let to_binding (type_index, function_indices) =
      let to_union_binding fidx = {
        name = sprintf "f%d" fidx ;
        value = T.TypeSymbol (env_name fidx) ;
      } in
      let union = {
        name = "u" ;
        value = T.Union (List.map function_indices ~f:to_union_binding) ;
      } in
      let tag = { name = "tag" ; value = T.TypeSymbol (type_tag_name type_index) } in
      {
        name = type_name type_index ;
        value = T.Structure [tag ; union] ;
      } in
    List.map types_to_functions' ~f:to_binding in

  T.{
    types = environments @ function_tags @ function_unions;
    procedures = [] ;
    main = [] ;
  }
