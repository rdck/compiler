open Core
open Symbol
open Types
open ThreeAddress

let inc r = Count (Inc, r)
let dec r = Count (Dec, r)
let register_map_of = Map.of_alist_exn (module Register)

let count_term environment arg instructions =
  (* separate return statement *)
  let body, return =
    List.split_while
      instructions
      ~f:
        begin
          function
          | Return _ -> false
          | _ -> true
        end
  in
  (* save return register for later *)
  let return_register =
    match List.hd_exn return with
    | Return r -> r
    | _ -> failwith "expected return statement at end of function"
  in
  (* set up a map of local register types *)
  let term_map =
    (* function environment *)
    let env = List.map environment ~f:(fun { name; value = t } -> Env name, t) in
    (* function argument *)
    let env =
      match arg with
      | None -> env
      | Some arg -> (Arg arg.Symbol.name, arg.value) :: env
    in
    (* local registers *)
    let block =
      List.filter_map body ~f:(function
        | Store (r, t, _) -> Some (r, t)
        | _ -> None)
    in
    register_map_of (env @ block)
  in
  (* lookup function *)
  let lookup_register = Map.find_exn term_map in
  (* issue increment for closure arguments *)
  let body =
    List.concat_map body ~f:(function
      | Store (_, _, Closure (_, args)) as instruction ->
        let args = List.filter args ~f:(fun r -> is_heap_type (lookup_register r)) in
        instruction :: List.map args ~f:inc
      | instruction -> [ instruction ])
  in
  (* issue increment for local stores *)
  let body =
    List.concat_map
      body
      ~f:
        begin
          function
          | Store (r, t, Read _) as instruction when is_heap_type t ->
            [ instruction; inc r ]
          | instruction -> [ instruction ]
        end
  in
  (* issue decrement when going out of scope *)
  let body =
    (* find heap stores that don't escape via return *)
    let stores =
      List.filter_map body ~f:(function
        | Store (r, t, _) when is_heap_type t ->
          if [%equal: register] r return_register then None else Some r
        | _ -> None)
    in
    (* issue drop for each heap store *)
    let drops = List.map stores ~f:dec in
    body @ drops
  in
  (* issue pair for function argument *)
  let body =
    match arg with
    | Some { name = arg_id; value = t } when is_heap_type t ->
      let arg_var = Arg arg_id in
      (inc arg_var :: body) @ [ dec arg_var ]
    | _ -> body
  in
  body @ return


let count_program program =
  let f { name; value = d } =
    binding
      name
      { env = d.env
      ; arg = d.arg
      ; body = count_term d.env (Some d.arg) d.body
      ; return_type = d.return_type
      }
  in
  { types = program.types
  ; terms = List.map program.terms ~f
  ; body = count_term [] None program.body
  }
