open Core
open Types
open Symbol
module S = Toponym (* source *)
module T = ThreeAddress (* target *)

type 'a compilation =
  { code : T.instruction list
  ; reg : 'a
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
      | S.Var (namespace, id) -> begin
        match namespace with
        | Arg -> { code = []; reg = T.Arg id }
        | Env -> { code = []; reg = T.Env id }
        | Loc -> { code = []; reg = T.Loc id }
      end
      | S.Cls (idx, args) ->
        let compiled_args = List.map args ~f:compile in
        let codes = List.map compiled_args ~f:project_code in
        let regs = List.map compiled_args ~f:project_reg in
        let sym = gensym () in
        { code =
            List.concat codes @ [ T.Store (sym, note, T.Closure (T.closure idx regs)) ]
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
      | S.Mat (control, cases) ->
        let symbol = gensym () in
        let { code = control_code; reg = control_register } = compile control in
        let cases =
          let compile_case S.{ code; data } =
            let compiled_data = List.map data ~f:compile in
            { code = List.concat (List.map compiled_data ~f:project_code)
            ; reg = T.closure code (List.map compiled_data ~f:project_reg)
            }
          in
          List.map cases ~f:compile_case
        in
        let cases_code = List.concat (List.map cases ~f:project_code) in
        let closures = List.map cases ~f:project_reg in
        let store = T.[ Store (symbol, note, Mat (control_register, closures)) ] in
        { code = control_code @ cases_code @ store; reg = symbol }
      | S.Let (id, e, b) ->
        let { code = ec; reg = er } = compile e in
        let { code = bc; reg = br } = compile b in
        let store = T.(Store (Loc id, e.S.note, Read er)) in
        { code = ec @ [ store ] @ bc; reg = br }
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
