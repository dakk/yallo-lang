open Ttype
open Env
open Expr

module Ast_ttype = Ttype
module Ast_env = Env
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal

type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * Env.contract) list;
  ifaces:      (iden * Env.entry_sig list) list;
} [@@deriving show {with_path = false}]
