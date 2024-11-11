include module type of SymbolData

val binding : 'k -> 'v -> ('k, 'v) binding
val binding_name : ('k, 'v) binding -> 'k
val binding_value : ('k, 'v) binding -> 'v
val pair_of_binding : ('k, 'v) binding -> 'k * 'v

(* uses polymorphic equality *)
val lookup : ('k, 'v) bindings -> 'k -> 'v option
val lookup_exn : ('k, 'v) bindings -> 'k -> 'v
