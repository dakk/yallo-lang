type iden = string [@@deriving show {with_path = false}]

type ptype = 
  | PTBase of string                  (* type name *)
  | PTTuple of ptype list             (* tuple of other types *)
  | PTRecord of (string * ptype) list (* record is (iden * type) list *)
  | PTCont of string * ptype          (* container type * inner_type *)
  | PTEnum of string list
  [@@deriving show {with_path = false}]

(* identifier * (iden * type) list of parameters * modifier list *)
type signature = iden * (iden * ptype) list * iden list [@@deriving show {with_path = false}]

(* a value, the type will be defined by declaration *)
type pvalue = 
  | PVEmpty
  | PVNone
  | PVSome of pvalue
  | PVTyped of pvalue * ptype
  [@@deriving show {with_path = false}]

type statement =
  | PSSkip
  [@@deriving show {with_path = false}]

(* contract field: iden * type * initial value *)
type contract_field = iden * ptype [@@deriving show {with_path = false}]

(* contract entry: iden * params * commands *)
type contract_entry = iden * (iden * ptype) list * unit [@@deriving show {with_path = false}]

type contract_constructor = (iden * ptype) list * (iden * pvalue) list [@@deriving show {with_path = false}]

(* a declaration could be a type alias, a modifier, an interface or a contract *)
type declaration = 
  (* | DModifier of modifier_decl *)

  (* constant value *)
  | DConst of string * ptype * pvalue

  (* type declaration *)
  | DType of string * ptype

  (* path import *)
  | DImport of string

  (* identifier * extends * signatures *)
  | DInterface of {
    id: iden;
    extends: iden option;
    signatures: signature list;
  }

  (* identifier * implements * entrypoints *)
  | DContract of {
    id: iden;
    implements: iden option;
    fields: contract_field list;
    entries: contract_entry list;
    constructor: contract_constructor option;
  }

  (* pure function, iden * params * rettype * body? *)
  | DFunction of {
    id: iden;
    params: (iden * ptype) list;
    rettype: ptype;
    statements: statement list;
  }
[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]
