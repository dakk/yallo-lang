open Ast_ttype
open Ast_expr
open Ast_statement

type signature = iden * (iden * ttype) list * iden list [@@deriving show {with_path = false}]

(* contract field: iden * type * initial value *)
type contract_field = iden * ttype [@@deriving show {with_path = false}]

(* contract entry: iden * params * commands *)
type contract_entry = iden * (iden * ttype) list * statement list
(* [@@deriving show {with_path = false}] *)

type contract_constructor = (iden * ttype) list * (iden * expr) list [@@deriving show {with_path = false}]

type dconst = { 
  id: iden; 
  t: ttype; 
  v: expr; 
} [@@deriving show {with_path = false}]

type dfunction = {
  id: iden;
  params: (iden * ttype) list;
  rettype: ttype;
  statements: statement list;
} 
(* [@@deriving show {with_path = false}] *)

type dcontract = {
  id: iden;
  implements: iden option;
  fields: contract_field list;
  entries: contract_entry list;
  constructor: contract_constructor option;
} 
(* [@@deriving show {with_path = false}] *)

type dinterface = { 
  id: iden;
  extends: iden option;
  signatures: signature list;
} 
(* [@@deriving show {with_path = false}] *)


type decl =
  | Const of dconst
  | Interface of dinterface
  | Contract of dcontract
  | Function of dfunction
  (* [@@deriving show {with_path = false}] *)