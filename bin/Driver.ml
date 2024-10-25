open Core
open Compiler

(* compilation mode *)
type mode =
  | TAC
  | CMM

(* stores each stage of compilation *)
type compilation = {
  source        : string            ;
  lexical       : Token.token list  ;
  syntax        : STLC.program      ;
  elaboration   : Annotated.program ;
  apex          : Lifted.program    ;
  triple        : TAC.program       ;
  procedural    : Cmm.program       ;
}

let compile source =

  (* run compilation *)
  let lexical     = Lex.tokenize (Lexing.from_string source)        in
  let syntax      = Option.value_exn (Pratt.parse_program lexical)  in
  let elaboration = Annotate.annotate_exn syntax                    in
  let apex        = Lift.lift_program elaboration                   in
  let triple      = Translate.compile_program apex                  in
  let procedural  = CmmBackend.compile_program triple               in

  (* store each phase *)
  {
    source        ;
    lexical       ;
    syntax        ;
    elaboration   ;
    apex          ;
    triple        ;
    procedural    ;
  }

let represent compilation = function
  | TAC -> TAC.show_program compilation.triple
  | CMM -> Cmm.represent compilation.procedural

let driver mode path =

  (* determine mode *)
  let mode = match mode with
  | Some "tac" -> TAC
  | _ -> CMM in

  (* check path *)
  match Sys_unix.is_file_exn path with
  | true ->
    let compilation = compile (In_channel.read_all path) in
    printf "%s\n" (represent compilation mode)
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

let () = Command_unix.run ~version:"0.1" ~build_info:"build_info" command
