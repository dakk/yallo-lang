type iden = string [@@deriving show {with_path = false}]

type ptype = 
  | PTBuiltin of string                  (* type name *)
  | PTTuple of ptype list             (* tuple of other types *)
  | PTRecord of (string * ptype) list (* record is (iden * type) list *)
  | PTCont of string * ptype          (* container type * inner_type *)
  | PTEnum of string list
  | PTLambda of ptype * ptype
  [@@deriving show {with_path = false}]

(* identifier * (iden * type) list of parameters * modifier list *)
type signature = iden * (iden * ptype) list * iden list [@@deriving show {with_path = false}]

(* left operator could be an ident or a this.ident *)
and left_op = 
  | I of iden     (* i *)
  | S of iden     (* this.i *)
  | T of iden     (* Tezos.i *)
  | C of iden     (* Crypto.i *)
  [@@deriving show {with_path = false}]

and pexpr =
  | PEUnit
  | PENone
  | PEBool of bool
  | PENat of int 
  | PEInt of int 
  | PEMutez of int
  | PEString of string
  | PESome of pexpr
  | PEEnum of iden * string
  | PETyped of pexpr * ptype
  | PEList of pexpr list 
  | PEMap of (pexpr * pexpr) list
  | PETuple of pexpr list
  | PELambda of (iden * ptype) list * pexpr
  | PERecord of (iden * pexpr) list

  | PERef of iden
  | PESRef of iden
  | PETRef of iden
  | PECRef of iden

  (* aritmetic *)
  | PEAdd of pexpr * pexpr
  | PESub of pexpr * pexpr
  | PEMul of pexpr * pexpr
  | PEDiv of pexpr * pexpr
  | PEMod of pexpr * pexpr

  (* bool *)
  | PEAnd of pexpr * pexpr
  | PEOr of pexpr * pexpr
  | PENot of pexpr
  | PELt of pexpr * pexpr
  | PELte of pexpr * pexpr
  | PEGt of pexpr * pexpr
  | PEGte of pexpr * pexpr
  | PEEq of pexpr * pexpr
  | PENeq of pexpr * pexpr

  (* ifthenelse expression *)
  | PEIfThenElse of pexpr * pexpr * pexpr 

  | PEMatchWith of pexpr * (pexpr * pexpr) list

  (* function apply *)
  | PEDot of pexpr * iden
  | PEHt of iden * iden
  | PEApply of pexpr * pexpr list

  [@@deriving show {with_path = false}]


type pstatement =
  | PSConst of iden * ptype * pexpr
  | PSVar of iden * ptype
  | PSVarAssignTuple of (iden * ptype) list * pexpr  (* var (a:int,b:int,c:int) = (1,2,3); *)
  | PSVarAssign of iden * ptype * pexpr              (* var a:int = 12; *)
  | PSAssign of left_op * pexpr                      (* a = 12; *)
  | PSRecAssign of left_op * iden * pexpr            (* a.el = 12; | this.a.el = 12; *)
  | PSCall of left_op * iden * pexpr list            (* m.update(k, 12) *)
  | PSCallBuiltin of iden * pexpr list               (* assert(15) *)
  | PSIfThenElse of pexpr * pstatement list * pstatement list
  | PSIfThen of pexpr * pstatement list
  | PSSkip
  | PSReturn of pexpr
  [@@deriving show {with_path = false}]

(* contract field: iden * type * initial value *)
type contract_field = iden * ptype [@@deriving show {with_path = false}]

(* contract entry: iden * params * commands *)
type contract_entry = iden * (iden * ptype) list * pstatement list [@@deriving show {with_path = false}]

type contract_constructor = (iden * ptype) list * (iden * pexpr) list [@@deriving show {with_path = false}]

(* a declaration could be a type alias, a modifier, an interface or a contract *)
type declaration = 
  (* path import *)
  | DImport of string

  (* a modifier, a pure function that assert a proprierty *)
  (* | DModifier of {
    id: iden;
    params: (iden * ptype) list;
    expression: unit;
  } *)

  (* constant value *)
  | DConst of { 
    id: iden; 
    t: ptype; 
    v: pexpr; 
  }

  (* type declaration *)
  | DType of { 
    id: iden; 
    t: ptype; 
  }

  (* interface *)
  | DInterface of { 
    id: iden;
    extends: iden option;
    signatures: signature list;
  }

  (* contract *)
  | DContract of {
    id: iden;
    implements: iden option;
    fields: contract_field list;
    entries: contract_entry list;
    constructor: contract_constructor option;
  }

  (* pure function *)
  | DFunction of {
    id: iden;
    params: (iden * ptype) list;
    rettype: ptype;
    statements: pstatement list;
  }
[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]
