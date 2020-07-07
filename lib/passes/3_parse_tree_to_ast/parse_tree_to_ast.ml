open Translate_pdecl

let translate (p: Parse_tree.t): Ast.t = 
  let e = transform p Env.start_env in {
    consts = e.consts;
    contracts = e.contracts;
    ifaces = e.ifaces
  }
  