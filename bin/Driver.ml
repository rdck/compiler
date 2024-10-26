open Core
open Compiler
open Result.Let_syntax

type ir =
  | Source
  | Lexical
  | Syntax
  | Elaboration
  | Apex
  | Triple
  | Procedural

let read_file path =
  match Sys_unix.is_file_exn path with
  | true  -> return (In_channel.read_all path)
  | false -> Result.fail "invalid path"

let ir_path name =
  let extend extension = sprintf "%s.%s" name extension in
  function
    | Source      -> extend "source"
    | Lexical     -> extend "lexical"
    | Syntax      -> extend "syntax"
    | Elaboration -> extend "elaboration"
    | Apex        -> extend "apex"
    | Triple      -> extend "triple"
    | Procedural  -> extend "c"

let write path content =
  let out_channel = Out_channel.create path in
  Out_channel.fprintf out_channel "%s" content

let compile path output_table =

  (* read source file *)
  let%bind source = read_file path in

  (* determine name for output files *)
  let basename = path |> Filename.basename |> Filename.chop_extension in

  (* write an IR to the appropriate path *)
  let write_ir ir content =
    if output_table ir then write (ir_path basename ir) content in

  (* lexical analysis *)
  let%bind lexical = Lex.tokenize (Lexing.from_string source) in
  write_ir Lexical ([%show: Token.token list] lexical) ;

  (* parsing *)
  let%bind syntax = Pratt.parse_program lexical in
  write_ir Syntax ([%show: STLC.program] syntax) ;

  (* elaboration *)
  let%bind elaboration = Annotate.annotate_program syntax in
  write_ir Elaboration (Annotated.represent_program elaboration) ;

  (* lambda lifting *)
  let apex = Lift.lift_program elaboration in
  write_ir Apex (Lifted.show_program apex) ;

  (* translation *)
  let triple = Translate.compile_program apex in
  write_ir Triple (TAC.show_program triple) ;

  (* translation to procedural *)
  let procedural = CmmBackend.compile_program triple in
  write_ir Procedural (Cmm.represent procedural) ;

  return ()

let driver output_table path =

  match compile path output_table with
  | Ok ()         -> ()
  | Error message -> fprintf stderr "%s\n" message

let command =
  Command.basic
  ~summary:"compiler"
  ~readme:(fun () -> "")
  (

    (* define options *)
    let%map_open.Command elaboration = flag "-elaboration" no_arg ~doc:
      "write out elaborated syntax tree"
    and apex = flag "-apex" no_arg ~doc:
      "write out lifted syntax tree"
    and triple = flag "-triple" no_arg ~doc:
      "write out three address code"
    and path = anon ("path" %: string) in

    (* code to run with above options available *)
    fun () ->
      let output_table = function
        | Source      -> false
        | Lexical     -> false
        | Syntax      -> false
        | Elaboration -> elaboration
        | Apex        -> apex
        | Triple      -> triple
        | Procedural  -> true
      in driver output_table path
  )

let () = Command_unix.run ~version:"0.1" ~build_info:"build_info" command
