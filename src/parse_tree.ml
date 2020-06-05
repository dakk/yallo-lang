type ident = string [@@deriving show {with_path = false}]

type ptype = 
| PTBase of string (* type name *)
| PTTuple of string list (* tuple of other types *)
| PTCont of string * ptype (* container type * inner_type *)
[@@deriving show {with_path = false}]

(* identifier * (iden * type) list of parameters * modifier list *)
type signature = ident * (ident * ptype) list * ident list [@@deriving show]

(* a declaration could be a type alias, a modifier, an interface or a contract *)
type declaration = 
(* | DModifier of modifier_decl *)

(* enum type declaration *)
| DEnum of string * string list

(* type declaration *)
| DType of string * ptype

(* path import *)
| DImport of string

(* identifier * extends * signatures *)
| DInterface of ident * ident option * (signature list)

(* identifier * extends * implements * entrypoints *)
| DContract of ident * ident option * ident option * unit list

(* pure funciton, ident * params * rettype * body? *)
| DFunction of ident * (ident * ptype) list * ptype * unit

[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]