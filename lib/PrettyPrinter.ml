open Core

type associativity =
  | Left
  | Right

type precedence = int

type 'a operation =
  | Nullary
  | Unary   of precedence * 'a
  | Binary  of precedence * associativity * 'a * 'a
  | Nary    of 'a list

module type EXPRESSION = sig

  type t
  val structure : t -> t operation
  val node_text : t -> string

end

module type S = sig

  type expression
  val print : expression -> string

end

module Make (E : EXPRESSION) = struct

  type expression = E.t

  let print =

    let open E in
  
    (* wrap a string in parentheses when a condition is met *)
    let wrap (s : string) (condition : bool) =
      if condition then sprintf "(%s)" s else s in
  
    let rec show (p : precedence) (expr : expression) =
      let atom = node_text expr in
      match structure expr with
      | Nullary -> atom
      | Unary (p', e) ->
          let s = sprintf "%s%s" atom (show (p' - 1) e) in
          wrap s (p' <= p)
      | Binary (p', assoc, lhs, rhs) ->
          let (left, right) = match assoc with
          | Left  -> (p' - 1, p')
          | Right -> (p', p' - 1) in
          let s = sprintf "%s%s%s" (show left lhs) atom (show right rhs) in
          wrap s (p' <= p)
      | Nary xs ->
          let ss = List.map xs ~f:(fun e -> show 0 e) in
          let ss' = String.concat ~sep:"," ss in
          sprintf "%s [%s]" atom ss'
  
    in show 0

end
