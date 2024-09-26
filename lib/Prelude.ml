(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

open Core

type ('k, 'v) binding = {
  name  : 'k ;
  value : 'v ;
}
[@@deriving equal]

let show_binding fk fv { name ; value } =
  Format.asprintf "%a := %a" fk name fv value

let pp_binding fk fv f b =
  Format.fprintf f "%s" (show_binding fk fv b)

type ('k, 'v) bindings = ('k, 'v) binding list
[@@deriving equal, show]

let project_name binding = binding.name
let project_value binding = binding.value

let value_exn opt =
  Option.value_exn opt
