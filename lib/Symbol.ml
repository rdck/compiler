open Core
include SymbolData

let binding k v = { name = k; value = v }
let binding_name binding = binding.name
let binding_value binding = binding.value
let pair_of_binding { name; value } = name, value

let lookup gamma k =
  let predicate binding = Poly.equal k binding.name in
  Option.map (List.find gamma ~f:predicate) ~f:binding_value


let lookup_exn gamma k = Option.value_exn (lookup gamma k)
let represent_symbol s = sprintf "%d" s
