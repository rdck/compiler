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

let program_types S.{ functions ; body } =

  let definitions = List.map (Map.to_alist functions) ~f:snd in
  let bodies = List.map definitions ~f:(fun d -> d.body) in
  let ts = List.concat (List.map bodies ~f:block_types) in
  ts @ block_types body

let index_map types =
  let sorted = List.dedup_and_sort types ~compare:STLC.Ty.compare in
  let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
  Map.of_alist_exn (module STLC.Ty) indexed

let compile_program source =
  failwith "TODO"
