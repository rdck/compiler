(******************************************************************************)
(* LIFTED -> TAC *)
(******************************************************************************)

open Core

module S = Lifted (* source *)
module T = TAC    (* target *)

type compilation = {
  code : T.instruction list ;
  reg : T.register ;
}

let project_code x = x.code
let project_reg x = x.reg

let compile_program S.{ functions ; body } =

  let compile_expression expr =

    let counter = ref 0 in
    let gensym () =
      let out = !counter in
      counter := out + 1 ;
      T.Reg out in

    let rec compile S.{ expr ; note } =
      match expr with
      | S.Lit i ->
          let sym = gensym () in {
            code = [ T.Store (sym, STLC.Z64, T.Lit i) ] ;
            reg = sym ;
          }
      | S.Bin (op, lhs, rhs) ->
          let { code = lhc ; reg = lhr } = compile lhs in
          let { code = rhc ; reg = rhr } = compile rhs in
          let sym = gensym () in {
            code = lhc @ rhc @ [ T.Store (sym, STLC.Z64, T.Bin (op, lhr, rhr)) ] ;
            reg = sym ;
          }
      | S.Var S.Arg ->
          { code = [] ; reg = T.Arg }
      | S.Var (S.Env id) ->
          { code = [] ; reg = T.Env id }
      | S.Closure (idx, args) ->
          let compiled_args = List.map args ~f:compile in
          let codes = List.map compiled_args ~f:project_code in
          let regs = List.map compiled_args ~f:project_reg in
          let sym = gensym () in {
            code = List.concat codes @ [ T.Store (sym, note, T.Closure (idx, regs)) ] ;
            reg = sym ;
          }
      | S.App (f, x) ->
          let { code = fc ; reg = fr } = compile f in
          let { code = xc ; reg = xr } = compile x in
          let sym = gensym () in {
            code = fc @ xc @ [ T.Store (sym, note, T.Call (fr, xr)) ] ;
            reg = sym ;
          } in

    compile expr in


  let compile_definition S.{ env ; arg ; body } =
    let { code ; reg } = compile_expression body in
    T.{
      env = env ;
      arg = arg ;
      body = code @ [ T.Return reg ] ;
      return_type = body.note ;
    } in

  T.{
    functions = Map.map functions ~f:compile_definition ;
    body =
      let { code ; reg } = compile_expression body in
      code @ [ T.Return reg ] ;
  }
