type ('k, 'v) binding = {
  name  : 'k ;
  value : 'v ;
}
[@@deriving equal, show]

type ('k, 'v) bindings = ('k, 'v) binding list
[@@deriving equal, show]

val binding : 'k -> 'v -> ('k, 'v) binding

val project_name : ('k, 'v) binding -> 'k
val project_value : ('k, 'v) binding -> 'v

val value_exn : 'a option -> 'a
