open Core
open Prelude

module S = TAC (* source *)
module T = Cmm (* target *)

let allocation_pointer = "aptr"

let register_name = function
  | S.Reg idx -> sprintf "r%d" idx
  | S.Arg -> "arg"
  | S.Env id -> sprintf "env->%s" id (* hack *)

let var r = T.Var (register_name r)
let ass r = T.Assignable (var r)

let filter_store_register r' = function
  | S.Store (r, t, _) ->
      if [%equal: S.register] r r' then Some t else None
  | _ -> None

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
  let enum_item_name type_index function_index =
    sprintf "%s_%s" (type_name type_index) (env_name function_index) in
  let application_name index = sprintf "apply_t%d" index in

  let atomic_type t =
    match t with
    | STLC.Z64 -> T.TypeSymbol "int64_t"
    | STLC.Arrow _ -> T.TypeSymbol (type_name (lookup_type_index t)) in

  (* map from function index to environment type *)
  let environment_map =

    let environment def =
      let atomicize { name ; value } = { name ; value = atomic_type value } in
      let reference_counter = {
        name = "rc" ;
        value = T.TypeSymbol "int64_t" ;
      } in
      T.Structure (reference_counter :: (List.map def.S.env ~f:atomicize)) in

    Map.map source.S.functions ~f:environment in

  (* get an ordered list of names in a function environment *)
  let get_environment_names fidx =
    match Map.find_exn environment_map fidx with
    | Structure bindings ->
        List.map (List.tl_exn bindings) ~f:(fun b -> b.name)
    | _ -> failwith "function environment must be struct" in

  (* a list of structs representing each function environment *)
  let environments =
    let alist = Map.to_alist environment_map in
    let f (key, data) = {
      name = env_name key ;
      value = data ;
    } in
    List.map alist ~f:f in

  (*
  (* a list of structs representing each function environment *)
  let environments =

    let environment S.{ env ; arg ; body = _ ; return_type = _ } =
      let atomicize { name ; value } = { name ; value = atomic_type value } in
      let reference_counter = {
        name = "rc" ;
        value = T.TypeSymbol "int64_t" ;
      } in
      T.Structure (reference_counter :: (List.map env ~f:atomicize)) in

    let environment_binding (index, definition) = {
      name = env_name index ;
      value = environment definition ;
    } in

    List.map function_bindings ~f:environment_binding in
  *)

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
  
  let closure_structs =

    let structure t =
      let index = lookup_type_index t in
      let def = T.Structure [
        { name = "tag" ; value = T.TypeSymbol (type_tag_name index) } ;
        { name = "env" ; value = T.Pointer (T.TypeSymbol "void") } ;
      ] in
      { name = type_name index ; value = def } in

    List.map function_types ~f:structure in

  (* code gen *)

  let compile_op = function
    | STLC.Add -> T.Add
    | STLC.Sub -> T.Sub
    | STLC.Mul -> T.Mul
    | STLC.Exp -> failwith "TODO: exponentiation" in

  let compile_instructions instructions register_type =

    let compile_expression = function
      | S.Lit i -> T.Lit i
      | S.Bin (op, lhs, rhs) -> T.Bin (compile_op op, ass lhs, ass rhs)
      | S.Closure _ -> failwith "UNREACHABLE"
      | S.Call (f, x) ->
          let ft_index' = lookup_type_index (register_type f) in
          T.Call (application_name ft_index', [ ass f ; ass x ]) in

    let compile_instruction =
      function
        | S.Store (dest, t, Closure (fidx, args)) ->
            let rname = register_name dest in
            let tidx = lookup_type_index t in
            let register_decl = T.Declare (rname, atomic_type t) in
            let tag_assignment = T.Assign (
              T.Dot (T.Var rname, "tag"),
              T.Assignable (T.Var (enum_item_name tidx fidx))
            ) in
            let eptr_type = T.Pointer (T.TypeSymbol (env_name fidx)) in
            let declaration = T.Declare (allocation_pointer, eptr_type) in
            let malloc = T.Assign (
              T.Var allocation_pointer,
              T.Assignable (T.Var "malloc(sizeof(*aptr))") (* fix later *)
            ) in
            let env_assignment = T.Assign (
              T.Dot (T.Var rname, "env"),
              T.Assignable (T.Var "aptr")
            ) in
            let arg_assignment =
              let assign_arg name value =
                T.Assign (
                  T.Arrow (T.Var "aptr", name),
                  T.Assignable (T.Var (register_name value))
                ) in
              let env_names = get_environment_names fidx in
              List.map2_exn env_names args ~f:assign_arg in
            let block = T.Block (declaration :: malloc :: env_assignment :: arg_assignment) in
            [ register_decl ; tag_assignment ; block ]
        | S.Store (dest, t, v) ->
            let rname = register_name dest in
            let register_decl = T.Declare (rname, atomic_type t) in
            let assignment = T.Assign (T.Var rname, compile_expression v) in
            [ register_decl ; assignment ]
        | S.Return r -> [T.Return (T.Assignable (T.Var (register_name r)))] in

    List.map instructions ~f:compile_instruction in


  let apply_procedures =

    let apply_procedure function_type =

      let functions = functions_of_type function_type in
      let ft_index = lookup_type_index function_type in
      let domain = STLC.project_domain_exn function_type in
      let codomain = STLC.project_codomain_exn function_type in

      let to_case fidx =

        let fdef = Map.find_exn source.S.functions fidx in
        let filter_arg_type id { name ; value } =
          if String.equal id name then Some value else None in
        let get_register_type = function
          | S.Reg _ as r -> List.find_map_exn fdef.S.body ~f:(filter_store_register r)
          | S.Arg -> domain
          | S.Env id -> List.find_map_exn fdef.S.env ~f:(filter_arg_type id) in

        let env_decl = T.Declare ("env", Pointer (TypeSymbol (env_name fidx))) in
        let env_defi = T.Assign (T.Var "env", T.Assignable (T.Var "fp.env")) (* hack *) in
        let body = List.concat (compile_instructions fdef.S.body get_register_type) in

        T.{
          tag = T.Assignable (T.Var (enum_item_name ft_index fidx)) ;
          body = env_decl :: env_defi :: body ;
        } in

      let cases = List.map functions ~f:to_case in

      {
        name = application_name ft_index ;
        value = T.{
          args = [
            { name = "fp" ; value = T.TypeSymbol (type_name ft_index) ; } ;
            { name = "arg" ; value = atomic_type domain ; } ;
          ] ;
          body = [
            T.Switch (T.Assignable (T.Var "fp.tag"), cases) ;
          ] ;
          return_type = atomic_type codomain ;
        } ;
      } in

    List.map function_types ~f:apply_procedure in

  (* duplicated logic with above *)
  let get_register_type = function
    | S.Reg _ as r -> List.find_map_exn source.S.body ~f:(filter_store_register r)
    | S.Arg -> failwith "unexpected arg in main"
    | S.Env _ -> failwith "unexpected env in main" in
  let main = List.concat (compile_instructions source.S.body get_register_type) in

  T.{
    types = function_enums @ closure_structs @ environments;
    procedures = apply_procedures ;
    main = main ;
  }
