(******************************************************************************)
(* SHARED TYPES *)
(******************************************************************************)

open Core

type identifier = string
[@@deriving equal, show, compare, sexp]

type ty =
  | TypeSymbol of identifier
  | Arrow of ty * ty
[@@deriving equal, show, compare, sexp]

let show_ty =
  let rec show p = function
    | TypeSymbol id -> id
    | Arrow (dom, cod) ->
        let dom' = show true dom in
        let cod' = show false cod in
        let s = sprintf "%s -> %s" dom' cod' in
        if p then sprintf "(%s)" s else s in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type constructor = {
  name : identifier ;
  parameter : ty ;
}
[@@deriving equal]

let show_constructor { name ; parameter } =
  sprintf "%s of %s" name ([%show: ty] parameter)

let pp_constructor f c =
  Format.fprintf f "%s" (show_constructor c)

(* should have at least one constructor *)
type type_specifier = constructor list
[@@deriving equal]

let show_type_specifier spec =
  let variants = List.map spec ~f:[%show: constructor] in
  String.concat ~sep:" | " variants

let pp_type_specifier f spec =
  Format.fprintf f "%s" (show_type_specifier spec)

let z64_symbol = "z64"
let z64 = TypeSymbol z64_symbol

module Ty = struct

  module T = struct

    type t = ty
    [@@deriving compare, sexp]

  end

  include T
  include Comparable.Make(T)

end

let project_domain = function
  | TypeSymbol _ -> None
  | Arrow (domain, _) -> Some domain

let project_domain_exn t = Option.value_exn (project_domain t)

let project_codomain = function
  | TypeSymbol _ -> None
  | Arrow (_, codomain) -> Some codomain 

let project_codomain_exn t = Option.value_exn (project_codomain t)

let is_symbol_type = function
  | TypeSymbol _ -> true
  | _ -> false

let is_arrow_type = function
  | Arrow _ -> true
  | _ -> false
