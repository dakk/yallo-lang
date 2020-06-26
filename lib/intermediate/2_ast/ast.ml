open Ttype
open Expr

module Ast_ttype = Ttype
module Ast_env = Env
module Ast_expr = Expr
module Ast_expr_traversal = Expr_traversal

type entry_sig = (iden * (iden * ttype) list)  [@@deriving show {with_path = false}]

type ctor = {
  arg: (iden * ttype) list;
  exprs: (iden * texpr) list;
} [@@deriving show {with_path = false}]


type entry = iden * (iden * ttype) list * texpr [@@deriving show {with_path = false}]

type contract = {
  fields: (iden * ttype) list;
  constructor: ctor;
  entries: entry list;
} [@@deriving show {with_path = false}]


type t = {
  consts:      (iden * texpr) list;
  contracts:   (iden * contract) list;
  ifaces:      (iden * entry_sig list) list;
} [@@deriving show {with_path = false}]
