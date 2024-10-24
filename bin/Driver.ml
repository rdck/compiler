open Core
open Compiler

type mode =
  | TAC
  | C

(* stores each stage of compilation *)
type compilation = {
  tokens    : Token.token list  ;
  ast       : STLC.program      ;
  annotated : Annotated.program ;
  lifted    : Lifted.program    ;
  tac       : TAC.program       ;
  cmm       : Cmm.program       ;
  output    : string            ;
}

let compile source = failwith ""

let represent =
  failwith ""

let driver mode path =

  (* determine mode *)
  let mode = match mode with
  | Some "tac" -> TAC
  | _ -> C in

  (* check path *)
  match Sys_unix.is_file_exn path with
  | true ->
    let compilation = compile path in
    printf "%s\n" (represent mode compilation)
  | false ->
    printf "invalid path\n"

let command =
  Command.basic
  ~summary:"compiler"
  ~readme:(fun () -> "more about the compiler")
  (
    let%map_open.Command mode = flag "-c" (optional string) ~doc:"compilation mode"
    and path = anon ("path" %: string) in
    fun () -> driver mode path
  )


(*
let compile source =
  match Sys_unix.is_file_exn path with
  | true ->
      let content = In_channel.read_all path in
      let lexed = Lex.tokenize (Lexing.from_string content) in
      let parsed = Option.value_exn (Pratt.parse_program lexed) in
      let annotated = Annotate.annotate_exn parsed in
      let lifted = Lift.lift_program annotated in
      let tac = Translate.compile_program lifted in
      let cmm = CmmBackend.compile_program tac in
      printf "%s\n" (Cmm.represent cmm)
  | false ->
      printf "invalid path\n"

let () = 

  let command =

    let param_handler path () =
      match Sys_unix.is_file_exn path with
      | true -> compile (In_channel.read_all path)

    let readme () = "compiler readme" in
    let param_spec = Command.Param.(anon ("path" %: string)) in
    let param_handler path () = compile path in
    Command.basic
    ~summary:"recurse center simply typed lambda calculus compiler"
    ~readme:readme
    (Command.Param.map param_spec ~f:param_handler) in

  Command_unix.run command
*)
