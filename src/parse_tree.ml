type ident = string [@@deriving show {with_path = false}]

type ptype = 
  | PTBase of string                  (* type name *)
  | PTTuple of ptype list             (* tuple of other types *)
  | PTRecord of (string * ptype) list (* record is (iden * type) list *)
  | PTCont of string * ptype          (* container type * inner_type *)
  | PTEnum of string list
  [@@deriving show {with_path = false}]

(* identifier * (iden * type) list of parameters * modifier list *)
type signature = ident * (ident * ptype) list * ident list [@@deriving show {with_path = false}]

(* contract field: ident * type * initial value *)
type contract_field = ident * ptype * unit [@@deriving show {with_path = false}]
type contract_entry = ident * (ident * ptype) list * unit [@@deriving show {with_path = false}]

(* a declaration could be a type alias, a modifier, an interface or a contract *)
type declaration = 
  (* | DModifier of modifier_decl *)

  (* type declaration *)
  | DType of string * ptype

  (* path import *)
  | DImport of string

  (* identifier * extends * signatures *)
  | DInterface of ident * ident option * (signature list)

  (* identifier * extends * implements * entrypoints *)
  | DContract of ident * ident option * ident option * contract_field list * contract_entry list

  (* pure funciton, ident * params * rettype * body? *)
  | DFunction of ident * (ident * ptype) list * ptype * unit (* command list *)
[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]