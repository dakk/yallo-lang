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
  | PVTezos of iden
  | PVString of string
  | PVInt of int 
  | PVBool of bool
  | PVEnum of iden * string
  | PVTuple of pvalue list
  | PVList of pvalue list
  | PVTyped of pvalue * ptype
  [@@deriving show {with_path = false}]

type pexpr = 
  | PEIf of pexpr * pexpr * pexpr
  | PEValue of pvalue
  | PEStorageRef of iden
  | PERef of iden
  | PEAdd of pexpr * pexpr
  | PESub of pexpr * pexpr
  | PEMul of pexpr * pexpr
  | PEDiv of pexpr * pexpr
  | PEMod of pexpr * pexpr
  | PENot of pexpr
  | PEEq of pexpr * pexpr
  | PELt of pexpr * pexpr
  | PELte of pexpr * pexpr
  | PEGt of pexpr * pexpr
  | PEGte of pexpr * pexpr
  | PEOr of pexpr * pexpr
  | PEAnd of pexpr * pexpr
  | PEApply of iden * pexpr list
  | PEContSize of pexpr
  | PEContHas of pexpr * pexpr
  | PEContGet of pexpr * pexpr
  [@@deriving show {with_path = false}]

type statement =
  | PSPush of iden * pexpr
  | PSUpdate of iden * pexpr * pexpr 
  | PSDecl of iden * ptype
  | PSDeclAssig of iden * ptype * pexpr
  | PSAssign of iden * pexpr
  | PSStorageAssign of iden * pexpr
  | PSIfThenElse of pexpr * statement list * statement list
  | PSIfThen of pexpr * statement list
  | PSReturn of pexpr
  | PSSkip
  [@@deriving show {with_path = false}]

(* contract field: iden * type * initial value *)
type contract_field = iden * ptype * pvalue [@@deriving show {with_path = false}]

(* contract entry: iden * params * commands *)
type contract_entry = iden * (iden * ptype) list * unit [@@deriving show {with_path = false}]

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
  | DInterface of iden * iden option * (signature list)

  (* identifier * implements * entrypoints *)
  | DContract of iden * iden option * contract_field list * contract_entry list

  (* pure function, iden * params * rettype * body? *)
  | DFunction of iden * (iden * ptype) list * ptype * statement list
[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]


let rec extract_contract pt contract = match pt with 
  | [] -> []
  | (DConst (a,b,c))::pt' -> (DConst (a,b,c))::(extract_contract pt' contract)
  | (DFunction (a,b,c,d))::pt' -> (DFunction (a,b,c,d))::(extract_contract pt' contract)
  (* | (DInterface (a,b,c))::pt' -> (DInterface (a,b,c))::(extract_contract pt' contract) *)
  | (DContract (id, _, d, e))::pt' when id=contract -> [DContract (id, None, d, e)]
  | _::pt' -> extract_contract pt' contract
;;