open Ttype
open Env
open Expr

module Ast_ttype = Ttype
module Ast_env = Env
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal

type t2 = 
  | Const of { id: iden; expr: texpr }
  | Contract of { id: iden; }
  | Interface of { id: iden; }

type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * Env.contract) list;
  ifaces:      (iden * Env.entry_sig list) list;
} [@@deriving show {with_path = false}]
