open Core
open Symbol
open Types
module S = ThreeAddress (* source *)
module T = Procedural (* target *)

(* naming scheme *)
let name_z64 = "int64_t"
let name_register index = sprintf "r%d" index
let name_type index = sprintf "T%d" index
let name_tag_type index = sprintf "T%dTag" index
let name_eval index = sprintf "apply_t%d" index
let name_drop index = sprintf "drop_t%d" index
let name_environment_type index = sprintf "F%d" index
let name_env_union index = sprintf "E%d" index
let name_function index = sprintf "f%d" index
let name_lambda t f = sprintf "%s_%s" (name_type t) (name_environment_type f)
let name_user_tag id = sprintf "%s_tag" id
let name_user_union id = sprintf "%s_union" id
let name_user_struct id = id
let name_argument = "arg"
let name_environment = "env"
let name_counter = "count"
let name_tag = "tag"
let name_void = "void"
let name_closure = "fp"
let name_union = "u"
let name_free = "free"
let name_match_closure = "matcher"

let register_var = function
  | S.Reg index -> T.Var (name_register index)
  | S.Arg _ -> T.Var name_argument
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
  (* TODO: duplicated with elaboration *)
  (* type symbol table *)
  let type_table =
    let alist = List.map source.S.types ~f:pair_of_binding in
    Map.of_alist_exn (module String) alist
  in
  (* type lookup function *)
  let lookup_type_exn = Map.find_exn type_table in
  let function_definitions = List.map source.S.terms ~f:binding_value in
  let term_map =
    let terms = List.map source.S.terms ~f:pair_of_binding in
    Map.of_alist_exn (module Int) terms
  in
  (* a list of all function types in the program *)
  let function_types =
    let multi = List.map function_definitions ~f:S.definition_type in
    List.dedup_and_sort multi ~compare:Ty.compare
  in
  (* a map from each function type to its index *)
  let type_to_index =
    let sorted = List.dedup_and_sort function_types ~compare:Ty.compare in
    let indexed = List.mapi sorted ~f:(Fn.flip Tuple2.create) in
    Map.of_alist_exn (module Ty) indexed
  in
  (* lookup a type's index via the above map *)
  let lookup_type_index = Map.find_exn type_to_index in
  let atomic_type t =
    let translate_type_symbol = function
      | "z64" -> T.TypeSymbol name_z64 (* TODO: standardize builtin type strings *)
      | id -> T.Pointer (T.TypeSymbol id)
    in
    match t with
    | TypeSymbol id -> translate_type_symbol id
    | Arrow _ -> T.Pointer (T.TypeSymbol (name_type (lookup_type_index t)))
  in
  (* map from function index to environment type *)
  let environment_map =
    let environment def =
      let atomicize { name; value } = { name; value = atomic_type value } in
      T.Structure (List.map def.S.env ~f:atomicize)
    in
    Map.map term_map ~f:environment
  in
  (* get an ordered list of names in a function environment *)
  let get_environment_names fidx =
    match Map.find_exn environment_map fidx with
    | Structure bindings -> List.map bindings ~f:(fun b -> b.name)
    | _ -> failwith "function environment must be struct"
  in
  (* a list of structs representing each function environment *)
  let environments =
    let alist = Map.to_alist environment_map in
    let bind (key, data) = { name = name_environment_type key; value = data } in
    List.map alist ~f:bind
  in
  (* filter functions by type *)
  let functions_of_type t =
    let filter { name = _; value = d } = [%equal: ty] t (S.definition_type d) in
    let bindings = List.filter source.S.terms ~f:filter in
    List.map bindings ~f:binding_name
  in
  (* a map from each function type to the list of functions inhabiting it *)
  let type_to_functions =
    let associate_functions t = t, functions_of_type t in
    let kvs = List.map function_types ~f:associate_functions in
    Map.of_alist_exn (module Ty) kvs
  in
  (* a map from each function type index to the list of functions inhabiting it *)
  let type_index_to_functions =
    Map.map_keys_exn (module Int) type_to_functions ~f:lookup_type_index
  in
  (* lookup function for above map *)
  let lookup_inhabitants = Map.find_exn type_index_to_functions in
  (* corresponding association list *)
  let type_index_with_functions = Map.to_alist type_index_to_functions in
  (* the enum for each function type *)
  let function_enums =
    let to_binding (type_index, function_indices) =
      let ids = List.map function_indices ~f:(name_lambda type_index) in
      { name = name_tag_type type_index; value = T.Enumeration ids }
    in
    List.map type_index_with_functions ~f:to_binding
  in
  let closure_unions =
    let union t =
      let index = lookup_type_index t in
      let inhabitants = lookup_inhabitants index in
      let def =
        T.Union
          (List.map inhabitants ~f:(fun inhabitant ->
             { name = name_function inhabitant
             ; value = T.TypeSymbol (name_environment_type inhabitant)
             }))
      in
      { name = name_env_union index; value = def }
    in
    List.map function_types ~f:union
  in
  let closure_structs =
    let structure t =
      let index = lookup_type_index t in
      let def =
        T.Structure
          [ { name = name_counter; value = T.TypeSymbol name_z64 }
          ; { name = name_tag; value = T.TypeSymbol (name_tag_type index) }
          ; { name = name_union; value = T.TypeSymbol (name_env_union index) }
          ]
      in
      { name = name_type index; value = def }
    in
    List.map function_types ~f:structure
  in
  (* enumerations for user types *)
  let user_enums =
    let build_enum { name; value = t } =
      let tags = List.map t ~f:(fun c -> c.name) in
      binding (name_user_tag name) (T.Enumeration tags)
    in
    List.map source.types ~f:build_enum
  in
  (* unions for user types *)
  let user_unions =
    let build_union { name; value = t } =
      let elements = List.map t ~f:(fun c -> binding c.name (atomic_type c.parameter)) in
      binding (name_user_union name) (T.Union elements)
    in
    List.map source.types ~f:build_union
  in
  (* structures for user types *)
  let user_structures =
    let build_struct { name; value = _ } =
      let spec =
        T.Structure
          [ { name = name_counter; value = T.TypeSymbol name_z64 }
          ; { name = name_tag; value = T.TypeSymbol (name_user_tag name) }
          ; { name = name_union; value = T.TypeSymbol (name_user_union name) }
          ]
      in
      binding (name_user_struct name) spec
    in
    List.map source.types ~f:build_struct
  in
  let compile_op = function
    | Syntax.Add -> T.Add
    | Syntax.Sub -> T.Sub
    | Syntax.Mul -> T.Mul
    | Syntax.Exp -> failwith "TODO: exponentiation"
  in
  let compile_instructions instructions register_type =
    let compile_expression = function
      | S.Lit i -> T.Lit i
      | S.Bin (op, lhs, rhs) ->
        T.Bin (compile_op op, register_value lhs, register_value rhs)
      | S.Call (f, x) ->
        let ft_index' = lookup_type_index (register_type f) in
        T.Call (name_eval ft_index', [ register_value f; register_value x ])
      | S.Closure _ -> failwith "UNREACHABLE"
      | S.Con _ -> failwith "UNREACHABLE"
      | S.Mat _ -> failwith "UNREACHABLE"
    in
    let compile_instruction = function
      | S.Store (dest, t, Closure (fidx, args)) ->
        let register_name = name_register (register_index_exn dest) in
        let register = register_var dest in
        let tidx = lookup_type_index t in
        let setup =
          T.
            [ Declare (register_name, atomic_type t)
            ; Assign
                ( register
                , Assignable (Var (sprintf "malloc(sizeof( *%s ))" register_name)) )
            ; Assign (Arrow (register, name_counter), Lit 1)
            ; Assign (Arrow (register, name_tag), Assignable (Var (name_lambda tidx fidx)))
            ]
        in
        let arg_assignment =
          let assign_arg name value =
            let arg_dest =
              T.(Dot (Dot (Arrow (register, name_union), name_function fidx), name))
            in
            T.(Assign (arg_dest, register_value value))
          in
          let environment_names = get_environment_names fidx in
          List.map2_exn environment_names args ~f:assign_arg
        in
        setup @ arg_assignment
      | S.Store (dest, t, Con (c, p)) ->
        let register_name = name_register (register_index_exn dest) in
        let register = register_var dest in
        T.
          [ Declare (register_name, atomic_type t)
          ; Assign
              (register, Assignable (Var (sprintf "malloc(sizeof( *%s ))" register_name)))
          ; Assign (Arrow (register, name_tag), Assignable (Var c))
          ; Assign (Dot (Arrow (register, name_union), c), Assignable (register_var p))
          ]
      | S.Store (dest, t, Mat (control, control_type, environment, cases)) ->
        let register_name = name_register (register_index_exn dest) in
        let register = register_var dest in
        let type_symbol = ty_symbol_exn control_type in
        let spec = lookup_type_exn type_symbol in
        let zipped = List.zip_exn spec cases in
        let gen_case ({ name; parameter }, symbol) =
          let closure_type = Types.Arrow (parameter, t) in
          let closure_type_index = lookup_type_index closure_type in
          let closure_var = T.Var name_match_closure in
          let function_tag = name_lambda closure_type_index symbol in
          let closure_setup =
            T.
              [ Declare (name_match_closure, atomic_type closure_type)
              ; Assign
                  ( closure_var
                  , Assignable (Var (sprintf "malloc(sizeof( *%s ))" name_match_closure))
                  )
              ; Assign (Arrow (closure_var, name_tag), Assignable (Var function_tag))
              ]
          in
          let arg_assignment =
            let assign_arg name value =
              let arg_dest =
                T.(
                  Dot (Dot (Arrow (closure_var, name_union), name_function symbol), name))
              in
              T.(Assign (arg_dest, register_value value))
            in
            let environment_names = get_environment_names symbol in
            List.map2_exn environment_names environment ~f:assign_arg
          in
          let parameter =
            T.(Assignable (Dot (Arrow (register_var control, name_union), name)))
          in
          T.
            { tag = Assignable (Var name)
            ; body =
                closure_setup
                @ arg_assignment
                @ [ Assign
                      ( register
                      , Call
                          ( name_eval closure_type_index
                          , [ Assignable closure_var; parameter ] ) )
                  ]
            }
        in
        T.
          [ Declare (register_name, atomic_type t)
          ; Switch
              ( Assignable (Arrow (register_var control, name_tag))
              , List.map zipped ~f:gen_case )
          ]
      | S.Store (dest, t, v) ->
        let register_name = name_register (register_index_exn dest) in
        let register = register_var dest in
        let register_decl = T.Declare (register_name, atomic_type t) in
        let assignment = T.Assign (register, compile_expression v) in
        [ register_decl; assignment ]
      | S.Return r -> [ T.Return (register_value r) ]
      | S.Count (op, r) ->
        let count = T.(Arrow (register_var r, name_counter)) in
        T.
          [ (match op with
             | Inc -> Assign (count, Bin (Add, Assignable count, Lit 1))
             | Dec ->
               let rt = lookup_type_index (register_type r) in
               Effect (Call (name_drop rt, [ Assignable (register_var r) ])))
          ]
    in
    List.map instructions ~f:compile_instruction
  in
  let apply_procedures =
    let apply_procedure function_type =
      let functions = functions_of_type function_type in
      let ft_index = lookup_type_index function_type in
      let domain = ty_domain_exn function_type in
      let codomain = ty_codomain_exn function_type in
      let to_case fidx =
        let fdef = Map.find_exn term_map fidx in
        let filter_arg_type id { name; value } =
          if String.equal id name then Some value else None
        in
        let get_register_type = function
          | S.Reg _ as r -> List.find_map_exn fdef.S.body ~f:(store_type r)
          | S.Arg _ -> domain
          | S.Env id -> List.find_map_exn fdef.S.env ~f:(filter_arg_type id)
        in
        let env_decl =
          T.Declare (name_environment, Pointer (TypeSymbol (name_environment_type fidx)))
        in
        let env_defi =
          T.Assign
            ( T.Var name_environment
            , T.Address T.(Dot (Arrow (Var name_closure, name_union), name_function fidx))
            )
        in
        let body = List.concat (compile_instructions fdef.S.body get_register_type) in
        T.
          { tag = T.Assignable (T.Var (name_lambda ft_index fidx))
          ; body = env_decl :: env_defi :: body
          }
      in
      let cases = List.map functions ~f:to_case in
      { name = name_eval ft_index
      ; value =
          T.
            { args =
                [ { name = name_closure; value = atomic_type function_type }
                ; { name = name_argument; value = atomic_type domain }
                ]
            ; body =
                [ T.Switch (T.Assignable (T.Arrow (T.Var name_closure, name_tag)), cases)
                ]
            ; return_type = atomic_type codomain
            }
      }
    in
    List.map function_types ~f:apply_procedure
  in
  (* compile drop procedures *)
  let drop_procedures =
    (* compile drop procedure for function type *)
    let drop_procedure function_type =
      let functions = functions_of_type function_type in
      let type_index = lookup_type_index function_type in
      let closure = T.Var name_closure in
      let count_assignable = T.(Arrow (closure, name_counter)) in
      let tag = T.(Arrow (closure, name_tag)) in
      (* compile case for function *)
      let compile_case fidx =
        (* TODO: factor out (shared with apply procedure) *)
        let env_decl =
          T.Declare (name_environment, Pointer (TypeSymbol (name_environment_type fidx)))
        in
        let env_defi =
          T.(
            Assign
              ( Var name_environment
              , Address (Dot (Arrow (Var name_closure, name_union), name_function fidx))
              ))
        in
        let function_definition = Map.find_exn term_map fidx in
        let environment = function_definition.S.env in
        let environment =
          List.filter environment ~f:(fun { name = _; value = t } -> is_arrow_type t)
        in
        let drop_statements =
          List.map environment ~f:(fun { name; value = t } ->
            let type_index = lookup_type_index t in
            let argument = T.(Assignable (Arrow (Var name_environment, name))) in
            T.(Effect (Call (name_drop type_index, [ argument ]))))
        in
        T.
          { tag = Assignable (Var (name_lambda type_index fidx))
          ; body = [ env_decl; env_defi ] @ drop_statements
          }
      in
      (* switch statement *)
      let switch = T.(Switch (Assignable tag, List.map functions ~f:compile_case)) in
      (* free call *)
      let free = T.(Effect (Call (name_free, [ Assignable closure ]))) in
      (* function body *)
      let body =
        T.
          [ Assign (count_assignable, Bin (Sub, Assignable count_assignable, Lit 1))
          ; If (Bin (LEQ, Assignable count_assignable, Lit 0), Block [ switch; free ])
          ]
      in
      T.
        { name = name_drop type_index
        ; value =
            { args = [ { name = name_closure; value = atomic_type function_type } ]
            ; body
            ; return_type = TypeSymbol name_void
            }
        }
    in
    List.map function_types ~f:drop_procedure
  in
  (* duplicated logic with above *)
  let get_register_type = function
    | S.Reg _ as r -> List.find_map_exn source.S.body ~f:(store_type r)
    | S.Arg _ -> failwith "unexpected arg in main"
    | S.Env _ -> failwith "unexpected env in main"
  in
  let main = List.concat (compile_instructions source.S.body get_register_type) in
  T.
    { types =
        user_enums
        @ user_unions
        @ user_structures
        @ function_enums
        @ environments
        @ closure_unions
        @ closure_structs
    ; procedures = drop_procedures @ apply_procedures
    ; main
    }
