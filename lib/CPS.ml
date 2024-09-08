(******************************************************************************)
(* CPS CONVERSION                                                             *)
(******************************************************************************)

open Core

type identifier = string
type 'a binding = identifier * 'a
type 'a environment = ('a binding) list

type arithmetic_operator =
  | Add
  | Sub
  | Mul

type expression =
  | Var of identifier
  | Abs of identifier * expression
  | App of expression * expression

let var id = Var id
let abs id e = Abs (id, e)
let app e1 e2 = App (e1, e2)

type aexp =
  | AAbs of identifier list * cexp
  | AVar of identifier
and cexp =
  | AApp of aexp * aexp list

let aabs ids e = AAbs (ids, e)
let avar id = AVar id
let aapp e es = AApp (e, es)

let sym_counter = ref 0
let gensym prefix =
  let r = !sym_counter in
  sym_counter := r + 1 ; prefix ^ (Int.to_string r)

let rec m = function
  | Abs (id, e) ->
      let k = gensym "k" in
      let args = [ id ; k ] in
      aabs args (t e (avar k))
  | Var id -> avar id
  | App (_, _) -> failwith "unexpected application in m"
and t (expr : expression) (k : aexp) =
  match expr with
  | Abs (_, _) -> aapp k [(m expr)]
  | Var _ -> aapp k [(m expr)]
  | App (f, e) ->
      let f' = gensym "f" in
      let e' = gensym "e" in
      t f (aabs [f'] (t e (aabs [e'] (aapp (avar f') [avar e' ; k]))))

let sjoin a b = a ^ " " ^ b

let pretty_args ids =
  List.fold ids ~init:"" ~f:sjoin

let rec apretty = function
  | AAbs (ids, e) -> "(λ " ^ pretty_args ids ^ " . " ^ cpretty e ^ ")"
  | AVar id -> id
and cpretty = function
  | AApp (f, es) ->
      let es' = List.map es ~f:(fun a -> apretty a) in
      let s = List.fold es' ~init:"" ~f:sjoin in
      "(" ^ apretty f ^ " " ^ s ^ ")"

let ex_00 = app (var "g") (var "a")

let transformed = t ex_00 (avar "halt")
