open Core

let concat_map xs ~f ~sep = String.concat (List.map xs ~f:f) ~sep:sep
