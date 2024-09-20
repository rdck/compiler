(******************************************************************************)
(* SIMPLY TYPED LAMBDA CALCULUS                                               *)
(******************************************************************************)

type identifier = string
[@@deriving equal, show]

type ty =
  | Int
  | Arrow of ty * ty
[@@deriving equal, show]

type binop =
  | Add
  | Sub
  | Mul
  | Exp
[@@deriving equal, show]

type 'a binding = {
  name : identifier ;
  value : 'a ;
}
[@@deriving equal, show]

type expression =
  | Lit of int
  | Bin of binop * expression * expression
  | Var of identifier
  | App of expression * expression
  | Abs of ty binding * expression
[@@deriving equal, show]
