open Core
open Prelude
open STLC

let ztz = Arrow (Z64, Z64)
let t0 = Arrow (Z64, ztz)
let t1 = Arrow (ztz, Z64)
let t2 = Arrow (Z64, t0)
let t3 = Arrow (t2, t2)

let bind s t = { name = s ; value = t }
let add = Abs (bind "a" Z64, Abs ((bind "b" Z64), Bin (Add, Var "a", Var "b")))
let add_one = App (add, Lit 1)
let three = App (add_one, Lit 2)

let e0 = Bin (Add, Bin (Add, Lit 1, Lit 2), Lit 3) (* 1 + 2 + 3 *)
let e1 = Bin (Add, Lit 1, Bin (Add, Lit 2, Lit 3)) (* 1 + (2 + 3) *)
let e2 = Bin (Add, Lit 1, Bin (Mul, Lit 2, Lit 3)) (* 1 + 2 * 3 *)
let e3 = Bin (Add, Bin (Mul, Lit 1, Lit 2), Lit 3) (* 1 * 2 + 3 *)
let e4 = Bin (Mul, Bin (Add, Lit 2, Lit 3), Lit 4) (* (2 + 3) * 4 *)
let e5 = Bin (Mul, Lit 2, Bin (Add, Lit 3, Lit 4)) (* 2 * (3 + 4) *)

let e6 = App (App (Var "f", Lit 2), Lit 3) (* f 2 3 *)
let e7 = Bin (Mul, App (Var "f", Lit 2), Lit 3) (* f 2 * 3 *)
let e8 = App (Var "f", Bin (Mul, Lit 2, Lit 3)) (* f (2 * 3 *)
let e9 = Abs (bind "x" Z64, Bin (Mul, Var "x", Lit 2)) (* λ x : Z . x * 2 *)
let e10 = App (e9, Lit 3) (* (λ x : Z . x * 2) 3 *)
let e11 = App (e9, e5)

let e12 = Bin (Sub, Bin (Sub, Lit 2, Lit 3), Lit 4)
let e13 = Bin (Sub, Lit 2, Bin (Sub, Lit 3, Lit 4))
