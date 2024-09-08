(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

(* [@@@warning "-32"] *)
open Core

type identifier = string
[@@deriving equal, show]

type ty =
  | Int
  | Arrow of ty * ty
[@@deriving equal]

let show_ty =
  let rec show p = function
    | Int -> "Z64"
    | Arrow (dom, cod) ->
        let dom' = show true dom in
        let cod' = show false cod in
        let s = Printf.sprintf "%s → %s" dom' cod' in
        if p then Printf.sprintf "(%s)" s else s in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type binop =
  | Add
  | Sub
  | Mul
[@@deriving equal]

let show_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"

let pp_binop f op = Format.fprintf f "%s" (show_binop op)

type 'a binding = {
  name : identifier ;
  value : 'a ;
}
[@@deriving equal]

let show_binding f { name ; value } =
  Format.asprintf "%s := %a" name f value

let pp_binding af f b =
  Format.fprintf f "%s" (show_binding af b)

type 'a environment = 'a binding list
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of ty binding * expression
[@@deriving equal]

let show_expression =
  let print = Printf.sprintf in
  let wrap s condition = if condition then print "(%s)" s else s in
  let rec show p = function
    | Lit i -> print "%d" i
    | Bin (op, lhs, rhs) ->
        let p' = match op with
        | Add -> 2
        | Sub -> 2
        | Mul -> 3 in
        let s = print "%s %s %s" (show (p' - 1) lhs) (show_binop op) (show p' rhs) in
        wrap s (p >= p')
    | Var id -> id
    | Abs ({ name ; value = domain }, body) ->
        let p' = 1 in
        let s = print "λ %s : %s . %s" name (show_ty domain) (show (p' - 1) body) in
        wrap s (p >= p')
    | App (f, x) ->
        let p' = 4 in
        let s = print "%s %s" (show (p' - 1) f) (show p' x) in
        wrap s (p >= p') in
  show 0

let pp_expression f e = Format.fprintf f "%s" (show_expression e)
