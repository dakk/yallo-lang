open Ttype
open Env
open Expr

module Ast_ttype = Ttype
module Ast_env = Env
module Ast_expr = Expr

type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * Env.contract) list;
  ifaces:      (iden * Env.entry_sig list) list;
} [@@deriving show {with_path = false}]
