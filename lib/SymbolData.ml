open Core

type identifier = string
[@@deriving equal, show, compare, sexp]

type symbol = int
[@@deriving equal, show]

type ('k, 'v) binding = {
  name  : 'k ;
  value : 'v ;
}
[@@deriving equal, show]

type ('k, 'v) bindings = ('k, 'v) binding list
[@@deriving equal, show]
