(******************************************************************************)
(* SHARED TYPES *)
(******************************************************************************)

open Core
include TypeData

let represent_ty =
  let rec show p = function
    | TypeSymbol id -> id
    | Arrow (dom, cod) ->
      let s = sprintf "%s -> %s" (show true dom) (show false cod) in
      if p then sprintf "(%s)" s else s
  in
  show false


let represent_constructor { name; parameter } =
  sprintf "%s of %s" name (represent_ty parameter)


let represent_type_specifier spec =
  String.concat ~sep:" | " (List.map spec ~f:represent_constructor)


let z64_symbol = "z64"
let z64 = TypeSymbol z64_symbol

module Ty = struct
  module T = struct
    type t = ty [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let ty_domain = function
  | TypeSymbol _ -> None
  | Arrow (domain, _) -> Some domain


let ty_domain_exn t = Option.value_exn (ty_domain t)

let ty_codomain = function
  | TypeSymbol _ -> None
  | Arrow (_, codomain) -> Some codomain


let ty_codomain_exn t = Option.value_exn (ty_codomain t)

let is_symbol_type = function
  | TypeSymbol _ -> true
  | _ -> false


let is_arrow_type = function
  | Arrow _ -> true
  | _ -> false


let ty_symbol = function
  | TypeSymbol id -> Some id
  | _ -> None


let ty_symbol_exn t = Option.value_exn ~message:"expected type symbol" (ty_symbol t)
