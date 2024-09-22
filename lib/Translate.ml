open Core

module S = LLC (* source *)
module T = TAC (* target *)

type compilation = {
  code : T.instruction list ;
  reg : T.register ;
}

let project_code x = x.code
let project_reg x = x.reg

let rec compile_expression table gensym = function
  | S.Lit i ->
      let sym = gensym () in {
        code = [ T.Store (sym, STLC.Int, T.Lit i) ] ;
        reg = sym ;
      }
  | S.Bin (op, lhs, rhs) ->
      let { code = lhc ; reg = lhr } = compile_expression table gensym lhs in
      let { code = rhc ; reg = rhr } = compile_expression table gensym rhs in
      let sym = gensym () in {
        code = lhc @ rhc @ [ T.Store (sym, STLC.Int, T.Bin (op, lhr, rhr)) ] ;
        reg = sym ;
      }
  | S.Var id ->
      { code = [] ; reg = T.Var id ; }
  | S.Closure (idx, args) ->
      let args' = List.map args ~f:(compile_expression table gensym) in
      let codes = List.map args' ~f:project_code in
      let regs = List.map args' ~f:project_reg in
      let defn = Map.find_exn table idx in
      let { STLC.name = _ ; STLC.value = domain } = defn.S.arg in
      let t = STLC.Arrow (domain, defn.S.return_type) in
      let sym = gensym () in {
        code = List.concat codes @ [ T.Store (sym, t, T.Closure (idx, regs)) ] ;
        reg = sym ;
      }
  | S.App (f, x) ->
      let { code = fc ; reg = fr } = compile_expression table gensym f in
      let { code = xc ; reg = xr } = compile_expression table gensym x in
      let sym = gensym () in {
        code = fc @ xc @ [ T.Store (sym, STLC.Int, T.Call (fr, xr)) ] ;
        reg = sym ;
      }
