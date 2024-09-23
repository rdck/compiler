(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

open Core

type ('k, 'v) binding = {
  name  : 'k ;
  value : 'v ;
}
[@@deriving equal]

let show_binding f { name ; value } =
  Format.asprintf "%s := %a" name f value

let pp_binding af f b =
  Format.fprintf f "%s" (show_binding af b)

let value_exn opt =
  Option.value_exn opt
