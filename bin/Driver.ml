open Core
open Compiler

type ir =
  | Source
  | Lexical
  | Syntax
  | Elaboration
  | Apex
  | Triple
  | Procedural
  | C99

let read_file path =
  match Sys_unix.is_file_exn path with
  | true  -> Result.return (In_channel.read_all path)
  | false -> Or_error.error_string "invalid path"

let ir_path name =
  let extend extension = sprintf "%s.%s" name extension in
  function
    | Source      -> extend "source"
    | Lexical     -> extend "lexical"
    | Syntax      -> extend "syntax"
    | Elaboration -> extend "elaboration"
    | Apex        -> extend "apex"
    | Triple      -> extend "triple"
    | Procedural  -> extend "procedural"
    | C99         -> extend "c"

let write path content =
  let out_channel = Out_channel.create path in
  Out_channel.fprintf out_channel "%s" content

let compile path output_table =

  (* write content to the appropriate path *)
  let write_ir ir content =
    if output_table ir then write (ir_path path ir) content in

  let open Result.Let_syntax in

  (* read source file *)
  let%bind source = read_file path in

  (* lexical analysis *)
  let%bind lexical = Lex.tokenize (Lexing.from_string source) in
  write_ir Lexical ([%show: Token.token list] lexical) ;

  (* parsing *)
  (* let%bind syntax = Pratt.parse_program lexical in *)

  failwith "TODO"

  (*
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
  *)

let driver output_table path =

  (* check path *)
  match Sys_unix.is_file_exn path with
  | true  ->
      begin match compile (In_channel.read_all path) output_table with
      | Ok x    -> printf "in ok"
      | Error e -> printf "in error"
      end
  | false -> printf "invalid path\n"

let command =
  Command.basic
  ~summary:"compiler"
  ~readme:(fun () -> "more about the compiler")
  (

    (* define options *)
    let%map_open.Command mode = flag "-c" (optional string) ~doc:"compilation mode"
    and path = anon ("path" %: string) in

    (* code to run with above options available *)
    fun () ->
      let output_table = function
        | Source      -> false
        | Lexical     -> false
        | Syntax      -> false
        | Elaboration -> false
        | Apex        -> false
        | Triple      -> false
        | Procedural  -> false
        | C99         -> false
      in driver output_table path
  )

let () = Command_unix.run ~version:"0.1" ~build_info:"build_info" command
