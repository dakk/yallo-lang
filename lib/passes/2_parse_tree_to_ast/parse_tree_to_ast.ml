open Parse_tree
open Ast
open Translate_pdecl

let translate (p: Parse_tree.t): t = 
  let e = transform p Ast_env.start_env in {
    consts = e.consts;
    contracts = e.contracts;
    ifaces = e.ifaces
  }
  