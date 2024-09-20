(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

open Core

let print = Printf.sprintf

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
        let s = print "%s → %s" dom' cod' in
        if p then print "(%s)" s else s in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal]

let show_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Exp -> "^"

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

type associativity =
  | Left
  | Right

type precedence = int

type operation =
  | Nullary
  | Unary   of precedence * expression
  | Binary  of precedence * associativity * expression * expression

let structure : expression -> operation = function
  | Lit _ -> Nullary
  | Bin (op, lhs, rhs) ->
      begin match op with
      | Add -> Binary (2, Left , lhs, rhs)
      | Sub -> Binary (2, Left , lhs, rhs)
      | Mul -> Binary (3, Left , lhs, rhs)
      | Exp -> Binary (4, Right, lhs, rhs)
      end
  | Var _ -> Nullary
  | App (f, x) -> Binary (5, Left, f, x)
  | Abs (_, body) -> Unary (1, body)

let node_text : expression -> string = function
  | Lit i -> print "%d" i
  | Bin (Add, _, _) -> " + "
  | Bin (Sub, _, _) -> " - "
  | Bin (Mul, _, _) -> " * "
  | Bin (Exp, _, _) -> " ^ "
  | Var id -> id
  | App _ -> " "
  | Abs ({ name ; value = domain }, _) ->
      print "λ %s : %s . " name (show_ty domain)

let show_expression : expression -> string =

  (* wrap a string in parentheses when a condition is met *)
  let wrap (s : string) (condition : bool) =
    if condition then print "(%s)" s else s in

  let rec show (p : precedence) (expr : expression) =
    let atom = node_text expr in
    match structure expr with
    | Nullary -> atom
    | Unary (p', e) ->
        let s = print "%s%s" atom (show (p' - 1) e) in
        wrap s (p' <= p)
    | Binary (p', assoc, lhs, rhs) ->
        let (left, right) = match assoc with
        | Left  -> (p' - 1, p')
        | Right -> (p', p' - 1) in
        let s = print "%s%s%s" (show left lhs) atom (show right rhs) in
        wrap s (p' <= p) in

  show 0

let pp_expression f e = Format.fprintf f "%s" (show_expression e)
