(******************************************************************************)
(* ANNOTATED LAMBDA CALCULUS                                                  *)
(******************************************************************************)

type identifier = STLC.identifier
[@@deriving equal, show]

type ty = STLC.ty
[@@deriving equal, show]

type binop = STLC.binop
[@@deriving equal, show]

type 'a expression =
  | Lit of int
  | Bin of binop * 'a note * 'a note
  | Var of identifier
  | App of 'a note * 'a note
  | Abs of identifier * 'a note
and 'a note = {
  expr : 'a expression ;
  note : 'a ;
}
[@@deriving equal]
