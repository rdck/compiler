open Core
open Compiler

let compile path =
  match Sys_unix.is_file_exn path with
  | true ->
      let content = In_channel.read_all path in
      let lexed = Lex.tokenize (Lexing.from_string content) in
      (* let _ = printf "%s" ([%show: Token.token list] lexed) in *)
      let parsed = Option.value_exn (Parse.parse_expr lexed) in
      let compiled = parsed.result
        |> Annotate.annotate_exn
        |> Lift.lift
        |> Translate.compile_program
        |> CmmBackend.compile_program in
      printf "%s\n" (Cmm.represent compiled)
  | false ->
      printf "invalid path"

let () = 
  let command =
    let summary = "recurse center simply typed lambda calculus compiler" in
    let readme () = "compiler!" in
    let param_spec = Command.Param.(anon ("path" %: string)) in
    let param_handler path () = compile path in
    Command.basic
      ~summary:summary
      ~readme:readme
      (Command.Param.map param_spec ~f:param_handler) in
  Command_unix.run command

(*
let _ =
  let compiled = Example.recursion
    |> Annotate.annotate_exn
    |> Lift.lift
    |> Translate.compile_program
    |> CmmBackend.compile_program in
  printf "%s\n" (Cmm.represent compiled)
*)
