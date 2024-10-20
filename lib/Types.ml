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
[@@deriving equal, show]

(* should have at least one constructor *)
type type_specifier = constructor list
[@@deriving equal, show]

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

