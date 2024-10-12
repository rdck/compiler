open Core
open Compiler

let compile path =
  match Sys_unix.is_file_exn path with
  | true ->
      let content = In_channel.read_all path in
      let lexed = Lex.tokenize (Lexing.from_string content) in
      let parsed = Option.value_exn (Pratt.parse_program lexed) in
      let compiled = parsed
        |> Annotate.annotate_exn
        |> Lift.lift
        |> Translate.compile_program
        |> CmmBackend.compile_program in
      printf "%s\n" (Cmm.represent compiled)
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
