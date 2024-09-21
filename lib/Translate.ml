open Core

module S = LLC (* source *)
module T = TAC (* target *)

type compilation = {
  code : T.instruction list ;
  reg : T.register ;
}

let project_code x = x.code
let project_reg x = x.reg

let rec compile_expression gensym = function
  | S.Lit i ->
      let sym = gensym () in {
        code = [ T.Store (sym, STLC.Int, T.Lit i) ] ;
        reg = sym ;
      }
  | S.Bin (op, lhs, rhs) ->
      let { code = lhc ; reg = lhr } = compile_expression gensym lhs in
      let { code = rhc ; reg = rhr } = compile_expression gensym rhs in
      let sym = gensym () in {
        code = lhc @ rhc @ [ T.Store (sym, STLC.Int, T.Bin (op, lhr, rhr)) ] ;
        reg = sym ;
      }
  | S.Var id ->
      { code = [] ; reg = T.Var id ; }
  | S.Closure (idx, args) ->
      let args' = List.map args ~f:(compile_expression gensym) in
      let codes = List.map args' ~f:project_code in
      let regs = List.map args' ~f:project_reg in
      let sym = gensym () in {
        code = List.concat codes @ [ T.Store (sym, STLC.Int, T.Closure (idx, regs)) ] ;
        reg = sym ;
      }
  | S.App (f, x) ->
      let { code = fc ; reg = fr } = compile_expression gensym f in
      let { code = xc ; reg = xr } = compile_expression gensym x in
      let sym = gensym () in {
        code = fc @ xc @ [ T.Store (sym, STLC.Int, T.Call (fr, xr)) ] ;
        reg = sym ;
      }
