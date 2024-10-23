open Core
open Compiler

let compile path =
  match Sys_unix.is_file_exn path with
  | true ->
      let content = In_channel.read_all path in
      let lexed = Lex.tokenize (Lexing.from_string content) in
      let parsed = Option.value_exn (Pratt.parse_program lexed) in
      printf "%s\n\n" ([%show: STLC.program] parsed) ;
      let annotated = Annotate.annotate_exn parsed in
      printf "%s\n\n" (Annotated.represent_program annotated) ;
      let lifted = Lift.lift_program annotated in
      let tac = Translate.compile_program lifted in
      let cmm = CmmBackend.compile_program tac in
      printf "%s\n" ([%show: TAC.program] tac)
      (* printf "%s\n" (Cmm.represent cmm) *)
  | false ->
      printf "invalid path\n"

let () = 
  let command =
    let readme () = "compiler readme" in
    let param_spec = Command.Param.(anon ("path" %: string)) in
    let param_handler path () = compile path in
    Command.basic
    ~summary:"recurse center simply typed lambda calculus compiler"
    ~readme:readme
    (Command.Param.map param_spec ~f:param_handler) in
  Command_unix.run command
