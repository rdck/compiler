(******************************************************************************)
(* PRIMTIVE C *)
(******************************************************************************)

open Core
open Prelude

type identifier = string
[@@deriving equal, show]

type index = int
[@@deriving equal, show]

type 'a symbol_table = (identifier, 'a, String.comparator_witness) Map.t

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
  | Structure of (identifier, ty) binding list
  | Union of (identifier, ty) binding list
[@@deriving equal]

let rec show_ty = function
  | Z64 -> "int64_t"
  | TypeSymbol id -> id
  | Enumeration _ -> failwith "TODO"
  | Structure bindings ->
      let f { name ; value } = sprintf "%s %s;" (show_ty value) name in
      let bs = String.concat (List.map bindings ~f:f) ~sep:" " in
      sprintf "struct { %s }" bs
  | _ -> failwith ""

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
}

type program = {
  types : ty symbol_table ;
  procedures : procedure symbol_table ;
  main : statement list ;
}

let represent { types ; procedures ; main } =
  let kvs = Map.to_alist types in
  let show_binding (id, ty) =
    sprintf "typedef %s %s;" ([%show: ty] ty) id in
  String.concat (List.map kvs ~f:show_binding) ~sep:"\n"
