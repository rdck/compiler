open Core
open Prelude

module S = TAC (* source *)
module T = Cmm (* target *)

type 'a table = (STLC.ty, 'a, STLC.Ty.comparator_witness) Map.t

type compilation = {
  statements : T.statement list ;
  value : T.expression ;
}

let compile_program source =

  let function_bindings = Map.to_alist source.S.functions in
  let function_definitions = List.map function_bindings ~f:snd in

  (* a list of all function types in the program *)
  let function_types =
    let multi = List.map function_definitions ~f:S.definition_type in
    List.dedup_and_sort multi ~compare:STLC.Ty.compare in

  (* a map from each function type to its index *)
  let type_to_index =
    let sorted = List.dedup_and_sort function_types ~compare:STLC.Ty.compare in
    let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
    Map.of_alist_exn (module STLC.Ty) indexed in

  (* lookup a type's index via the above map *)
  let lookup_type_index = Map.find_exn type_to_index in

  let type_name index     = sprintf "T%d" index in
  let env_name index      = sprintf "F%d" index in
  let type_tag_name index = sprintf "T%dTag" index in
  let function_name index = sprintf "f%d" index in
  let enum_item_name type_index function_index =
    sprintf "%s_%s" (type_name type_index) (env_name function_index) in
  let application_name index = sprintf "apply_t%d" index in

  let atomic_type t =
    match t with
    | STLC.Z64 -> T.Z64
    | STLC.Arrow (domain, codomain) ->
        T.TypeSymbol (type_name (lookup_type_index t)) in

  (* a list of structs representing each function environment *)
  let environments =

    let environment S.{ env ; arg ; body = _ ; return_type = _ } =
      let atomicize { name ; value } = { name ; value = atomic_type value } in
      T.Structure (List.map (arg :: env) ~f:atomicize) in

    let environment_binding (index, definition) = {
      name = env_name index ;
      value = environment definition ;
    } in

    List.map function_bindings ~f:environment_binding in

  (* filter functions by type *)
  let functions_of_type t =
    let filter (_, d) = [%equal: STLC.ty] t (S.definition_type d) in
    let bindings = List.filter function_bindings ~f:filter in
    List.map bindings ~f:fst in

  (* a map from each function type to the list of functions inhabiting it *)
  let type_to_functions =
    let associate_functions t = (t, functions_of_type t) in
    let kvs = List.map function_types ~f:associate_functions in
    Map.of_alist_exn (module STLC.Ty) kvs in

  (* lookup the functions inhabiting a type in the above map *)
  let lookup_type_functions = Map.find_exn type_to_functions in

  (* a map from each function type index to the list of functions inhabiting it *)
  let type_index_to_functions =
    Map.map_keys_exn (module Int) type_to_functions ~f:lookup_type_index in

  (* corresponding association list *)
  let type_index_with_functions = Map.to_alist type_index_to_functions in

  (* the enum for each function type *)
  let function_enums =

    let to_binding (type_index, function_indices) =
      let ids = List.map function_indices ~f:(enum_item_name type_index) in
      { name = type_tag_name type_index ; value = T.Enumeration ids } in

    List.map type_index_with_functions ~f:to_binding in

  (* the closure representation for each function type *)
  let closure_structs =

    let to_binding (type_index, function_indices) =
      let to_union_binding fidx = {
        name = function_name fidx ;
        value = T.TypeSymbol (env_name fidx) ;
      } in
      let union = {
        name = "u" ;
        value = T.Union (List.map function_indices ~f:to_union_binding) ;
      } in
      let tag = {
        name = "tag" ;
        value = T.TypeSymbol (type_tag_name type_index) ;
      } in
      {
        name = type_name type_index ;
        value = T.Structure [tag ; union] ;
      } in

    List.map type_index_with_functions ~f:to_binding in

  (*

  let register_name = function
    | S.Reg idx -> sprintf "r%d" idx
    | S.Var id -> id in

  let compile_op = function
    | STLC.Add -> T.Add
    | STLC.Sub -> T.Sub
    | STLC.Mul -> T.Mul
    | STLC.Exp -> failwith "TODO: exponentiation" in

  let var r = T.Var (register_name r) in

  let compile_expression = function
    | S.Lit i -> T.Lit i
    | S.Bin (op, lhs, rhs) ->
        T.Bin (compile_op op, var lhs, var rhs)
    | S.Closure (fidx, args) -> failwith ""
    | _ -> failwith "" in

  (* @rdk: this should just match on the value expression to handle the closure case *)
  let compile_instruction = function
    | S.Store (dest, t, value) ->
        let name = register_name dest in
        let declaration = T.Declare (name, atomic_type t) in
        let assignment = T.Assign (T.Var name, compile_expression value) in
        [ declaration ; assignment ]
    | _ -> failwith "" in
  
  let compile_instructions instructions =
    List.concat (List.map instructions ~f:compile_instruction) in

  (* the `apply` function for each function type *)
  let application_procedures =

    (* generate the `apply` function for a given function type *)
    let application_procedure function_type =

      let type_index = lookup_type_index function_type in
      let procedure_id = application_name type_index in

      let procedure =

        let to_case fidx =
          let definition = Map.find_exn source.S.functions fidx in
          T.{
            tag = T.Var (enum_item_name type_index fidx) ;
            body = compile_instructions definition.S.body ;
          } in

        let cases = List.map (lookup_type_functions function_type) ~f:to_case in
        let switch_statement = T.Switch (T.Arrow (T.Var "fp", T.Var "tag"), cases) in

        T.{
          arg = {
            name = "fp" ;
            value = T.Pointer (T.TypeSymbol (type_name type_index)) ;
          } ;
          body = [ switch_statement ] ;
          return_type = atomic_type (STLC.project_codomain_exn function_type) ;
        } in

      { name = procedure_id ; value = procedure } in

    List.map function_types ~f:application_procedure in

  *)

  T.{
    types = environments @ function_enums @ closure_structs;
    (* procedures = application_procedures ; *)
    procedures = [] ;
    main = [] ;
  }
