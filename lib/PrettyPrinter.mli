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

module Make (E : EXPRESSION) : S with type expression = E.t
