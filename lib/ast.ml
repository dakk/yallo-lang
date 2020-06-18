open Ast_ttype
open Ast_env
open Ast_expr
open Translate_pdecl

type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * Env.contract) list;
  ifaces:      (iden * Env.entry_sig list) list;
} [@@deriving show {with_path = false}]

let of_parse_tree (p: Parse_tree.t): t = 
  let e = transform p Env.start_env in {
    consts = e.consts;
    contracts = e.contracts;
    ifaces = e.ifaces
  }
  