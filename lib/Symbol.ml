include SymbolData

let binding k v = { name = k; value = v }
let binding_name binding = binding.name
let binding_value binding = binding.value
let pair_of_binding { name; value } = name, value
