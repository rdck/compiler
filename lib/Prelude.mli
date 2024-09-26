open Core

type ('k, 'v) binding = {
  name  : 'k ;
  value : 'v ;
}
[@@deriving equal, show]

val project_name : ('k, 'v) binding -> 'k
val project_value : ('k, 'v) binding -> 'v

val value_exn : 'a option -> 'a
