open Core
open Prelude
open Types

module S = ThreeAddress (* source *)
module T = Procedural   (* target *)

(* naming scheme *)
let name_z64 = "int64_t"
let name_register index = sprintf "r%d" index
let name_type index = sprintf "T%d" index
let name_tag_type index = sprintf "T%dTag" index
let name_eval index = sprintf "apply_t%d" index
let name_environment_type index = sprintf "F%d" index
let name_env_union index = sprintf "E%d" index
let name_function index = sprintf "f%d" index
let name_lambda t f = sprintf "%s_%s" (name_type t) (name_environment_type f)
let name_argument = "arg"
let name_allocation = "aptr"
let name_environment = "env"
let name_counter = "count"
let name_tag = "tag"
let name_void = "void"
let name_closure = "fp"
let name_union = "u"

let register_var = function
  | S.Reg index -> T.Var (name_register index)
  | S.Arg -> T.Var name_argument
  | S.Env id -> T.Arrow (T.Var name_environment, id)

let register_index_exn = function
  | S.Reg index -> index
  | _ -> failwith "expected register"

let register_value r = T.Assignable (register_var r)

(* extract the destination type of a store instruction *)
let store_type r = function
  | S.Store (r', t, _) -> if [%equal: S.register] r r' then Some t else None
  | _ -> None

let compile_program source =

  let function_definitions = List.map source.S.terms ~f:project_value in
  let term_map =
    let terms = List.map source.S.terms ~f:pair_of_binding in
    Map.of_alist_exn (module Int) terms in

  (* a list of all function types in the program *)
  let function_types =
    let multi = List.map function_definitions ~f:S.definition_type in
    List.dedup_and_sort multi ~compare:Ty.compare in

  (* a map from each function type to its index *)
  let type_to_index =
    let sorted = List.dedup_and_sort function_types ~compare:Ty.compare in
    let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
    Map.of_alist_exn (module Ty) indexed in

  (* lookup a type's index via the above map *)
  let lookup_type_index = Map.find_exn type_to_index in

  let atomic_type t =

    let translate_type_symbol = function
      | "z64" -> name_z64 (* TODO: standardize builtin type strings *)
      | id -> id in

    match t with
    | TypeSymbol id -> T.TypeSymbol (translate_type_symbol id)
    | Arrow _ -> T.Pointer (T.TypeSymbol (name_type (lookup_type_index t))) in

  (* map from function index to environment type *)
  let environment_map =

    let environment def =
      let atomicize { name ; value } = { name ; value = atomic_type value } in
      T.Structure (List.map def.S.env ~f:atomicize) in

    Map.map term_map ~f:environment in

  (* get an ordered list of names in a function environment *)
  let get_environment_names fidx =
    match Map.find_exn environment_map fidx with
    | Structure bindings ->
        List.map bindings ~f:(fun b -> b.name)
    | _ -> failwith "function environment must be struct" in

  (* a list of structs representing each function environment *)
  let environments =
    let alist = Map.to_alist environment_map in
    let bind (key, data) = {
      name = name_environment_type key ;
      value = data ;
    } in
    List.map alist ~f:bind in

  (* filter functions by type *)
  let functions_of_type t =
    let filter { name = _ ; value = d } = [%equal: ty] t (S.definition_type d) in
    let bindings = List.filter source.S.terms ~f:filter in
    List.map bindings ~f:project_name in

  (* a map from each function type to the list of functions inhabiting it *)
  let type_to_functions =
    let associate_functions t = (t, functions_of_type t) in
    let kvs = List.map function_types ~f:associate_functions in
    Map.of_alist_exn (module Ty) kvs in

  (* a map from each function type index to the list of functions inhabiting it *)
  let type_index_to_functions =
    Map.map_keys_exn (module Int) type_to_functions ~f:lookup_type_index in

  (* lookup function for above map *)
  let lookup_inhabitants = Map.find_exn type_index_to_functions in

  (* corresponding association list *)
  let type_index_with_functions = Map.to_alist type_index_to_functions in

  (* the enum for each function type *)
  let function_enums =

    let to_binding (type_index, function_indices) =
      let ids = List.map function_indices ~f:(name_lambda type_index) in
      { name = name_tag_type type_index ; value = T.Enumeration ids } in

    List.map type_index_with_functions ~f:to_binding in

  let closure_unions =

    let union t =
      let index = lookup_type_index t in
      let inhabitants = lookup_inhabitants index in
      let def = T.Union (List.map inhabitants ~f:(fun inhabitant -> {
        name = name_function inhabitant ;
        value = T.TypeSymbol (name_environment_type inhabitant) ;
      })) in
      { name = name_env_union index ; value = def } in

    List.map function_types ~f:union in
  
  let closure_structs =

    let structure t =
      let index = lookup_type_index t in
      let def = T.Structure [
        { name = name_tag         ; value = T.TypeSymbol (name_tag_type index)    } ;
        { name = name_union       ; value = T.TypeSymbol (name_env_union index)   } ;
      ] in
      { name = name_type index ; value = def } in

    List.map function_types ~f:structure in

  let compile_op = function
    | Syntax.Add -> T.Add
    | Syntax.Sub -> T.Sub
    | Syntax.Mul -> T.Mul
    | Syntax.Exp -> failwith "TODO: exponentiation" in

  let compile_instructions instructions register_type =

    let compile_expression = function
      | S.Lit i -> T.Lit i
      | S.Bin (op, lhs, rhs) ->
          T.Bin (compile_op op, register_value lhs, register_value rhs)
      | S.Closure _ -> failwith "UNREACHABLE"
      | S.Call (f, x) ->
          let ft_index' = lookup_type_index (register_type f) in
          T.Call (name_eval ft_index', [ register_value f ; register_value x ]) in

    let compile_instruction =
      function
        | S.Store (dest, t, Closure (fidx, args)) ->
            let register_name = name_register (register_index_exn dest) in
            let register = register_var dest in
            let tidx = lookup_type_index t in
            let setup = T.[
              Declare (register_name, atomic_type t) ;
              Assign (
                register,
                Assignable (Var (sprintf "malloc(sizeof( *%s ))" register_name))
              ) ;
              Assign (
                Arrow (register, name_tag),
                Assignable (Var (name_lambda tidx fidx))
              ) ;
            ] in
            let arg_assignment =
              let assign_arg name value =
                let arg_dest = T.(Dot (Dot (Arrow (register, name_union), name_function fidx), name)) in
                T.(Assign (arg_dest, register_value value)) in
              let environment_names = get_environment_names fidx in
              List.map2_exn environment_names args ~f:assign_arg in
            setup @ arg_assignment
        | S.Store (dest, t, v) ->
            let register_name = name_register (register_index_exn dest) in
            let register = register_var dest in
            let register_decl = T.Declare (register_name, atomic_type t) in
            let assignment = T.Assign (register, compile_expression v) in
            [ register_decl ; assignment ]
        | S.Return r -> [T.Return (register_value r)] in

    List.map instructions ~f:compile_instruction in


  let apply_procedures =

    let apply_procedure function_type =

      let functions = functions_of_type function_type in
      let ft_index = lookup_type_index function_type in
      let domain = project_domain_exn function_type in
      let codomain = project_codomain_exn function_type in

      let to_case fidx =

        let fdef = Map.find_exn term_map fidx in
        let filter_arg_type id { name ; value } =
          if String.equal id name then Some value else None in
        let get_register_type = function
          | S.Reg _ as r -> List.find_map_exn fdef.S.body ~f:(store_type r)
          | S.Arg -> domain
          | S.Env id -> List.find_map_exn fdef.S.env ~f:(filter_arg_type id) in

        let env_decl = T.Declare (
          name_environment,
          Pointer (TypeSymbol (name_environment_type fidx))
        ) in

        let env_defi = T.Assign (
          T.Var name_environment,
          T.Address (T.(Dot (Arrow (Var name_closure, name_union), name_function fidx)))
        ) in
        let body = List.concat (compile_instructions fdef.S.body get_register_type) in

        T.{
          tag = T.Assignable (T.Var (name_lambda ft_index fidx)) ;
          body = env_decl :: env_defi :: body ;
        } in

      let cases = List.map functions ~f:to_case in

      {
        name = name_eval ft_index ;
        value = T.{
          args = [
            (* { name = name_closure   ; value = T.TypeSymbol (name_type ft_index) ; } ; *)
            { name = name_closure   ; value = atomic_type function_type         ; } ;
            { name = name_argument  ; value = atomic_type domain                ; } ;
          ] ;
          body = [
            T.Switch (T.Assignable (T.Arrow (T.Var name_closure, name_tag)), cases) ;
          ] ;
          return_type = atomic_type codomain ;
        } ;
      } in

    List.map function_types ~f:apply_procedure in

  (* duplicated logic with above *)
  let get_register_type = function
    | S.Reg _ as r -> List.find_map_exn source.S.body ~f:(store_type r)
    | S.Arg -> failwith "unexpected arg in main"
    | S.Env _ -> failwith "unexpected env in main" in
  let main = List.concat (compile_instructions source.S.body get_register_type) in

  T.{
    types = function_enums @ environments @ closure_unions @ closure_structs ;
    procedures = apply_procedures ;
    main = main ;
  }
