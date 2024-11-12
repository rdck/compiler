open Core
open Types
open Symbol
module S = Toponym (* source *)
module T = ThreeAddress (* target *)

type compilation =
  { code : T.instruction list
  ; reg : T.register
  }

let project_code x = x.code
let project_reg x = x.reg

let compile_program S.{ types; terms; body } =
  let compile_expression expr =
    let counter = ref 0 in
    let gensym () =
      let out = !counter in
      counter := out + 1;
      T.Reg out
    in
    let rec compile S.{ expr; note } =
      match expr with
      | S.Lit i ->
        let sym = gensym () in
        { code = T.[ Store (sym, z64, Lit i) ]; reg = sym }
      | S.Bin (op, lhs, rhs) ->
        let { code = lhc; reg = lhr } = compile lhs in
        let { code = rhc; reg = rhr } = compile rhs in
        let sym = gensym () in
        { code = (lhc @ rhc @ T.[ Store (sym, z64, Bin (op, lhr, rhr)) ]); reg = sym }
      | S.Var (namespace, id) ->
        (match namespace with
         | Arg -> { code = []; reg = T.Arg id }
         | Env -> { code = []; reg = T.Env id }
         | Loc -> failwith "TODO")
      | S.Cls (idx, args) ->
        let compiled_args = List.map args ~f:compile in
        let codes = List.map compiled_args ~f:project_code in
        let regs = List.map compiled_args ~f:project_reg in
        let sym = gensym () in
        { code = List.concat codes @ [ T.Store (sym, note, T.Closure (idx, regs)) ]
        ; reg = sym
        }
      | S.App (f, x) ->
        let { code = fc; reg = fr } = compile f in
        let { code = xc; reg = xr } = compile x in
        let sym = gensym () in
        { code = fc @ xc @ [ T.Store (sym, note, T.Call (fr, xr)) ]; reg = sym }
      | S.Con (c, p) ->
        let { code = parameter_code; reg = parameter_register } = compile p in
        let sym = gensym () in
        let store = T.(Store (sym, note, Con (c, parameter_register))) in
        { code = parameter_code @ [ store ]; reg = sym }
      | S.Mat (control, environment, cases) ->
        let { code = control_code; reg = control_register } = compile control in
        let environment = List.map environment ~f:compile in
        (* We ignore generated code for these, because it should never exist. *)
        let environment_registers = List.map environment ~f:(fun c -> c.reg) in
        let sym = gensym () in
        let match_expression =
          T.(Mat (control_register, control.note, environment_registers, cases))
        in
        let mat = T.(Store (sym, note, match_expression)) in
        { code = control_code @ [ mat ]; reg = sym }
    in
    compile expr
  in
  let compile_definition S.{ env; arg; body } =
    let { code; reg } = compile_expression body in
    T.{ env; arg; body = code @ [ T.Return reg ]; return_type = body.note }
  in
  let terms =
    List.map terms ~f:(fun { name; value } -> binding name (compile_definition value))
  in
  T.
    { types
    ; terms
    ; body =
        (let { code; reg } = compile_expression body in
         code @ [ T.Return reg ])
    }
