(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type expression =
  | Var of identifier
  | Arrow of expression * expression
  | Dot of expression * expression
  | Apply of expression
  | Binop of expression * expression

type ty =
  | Z64
  | TypeSymbol of identifier
  | Enumeration of identifier list
  | Structure of (identifier, ty) bindings
  | Union of (identifier, ty) bindings
  | Pointer of ty
[@@deriving equal]

let rec show_ty = function
  | Z64 -> "int64_t"
  | TypeSymbol id -> id
  | Enumeration ids ->
      sprintf "enum { %s }" (String.concat ~sep:",\n" ids)
  | Structure bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      let bs = String.concat (List.map bindings ~f:f) ~sep:" " in
      sprintf "struct { %s }" bs
  | Union bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      let bs = String.concat (List.map bindings ~f:f) ~sep:" " in
      sprintf "union { %s }" bs
  | Pointer t -> sprintf "%s*" (show_ty t)

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type statement =
  | Declare of identifier * ty
  | Assign of expression * expression
  | Switch of expression * case list
  | Return of expression
and case = {
  tag : expression ;
  body : statement list ;
}

type procedure = {
  arg : (identifier, ty) binding ;
  body : statement list ;
  return_type : ty ;
}

type program = {
  types : (identifier, ty) bindings ;
  procedures : (identifier, procedure) bindings ;
  main : statement list ;
}

let represent { types ; procedures ; main } =
  let show_binding { name ; value } =
    sprintf "typedef %s %s;" ([%show: ty] value) name in
  String.concat (List.map types ~f:show_binding) ~sep:"\n"
