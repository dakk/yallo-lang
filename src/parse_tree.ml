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

(* a value, the type will be defined by declaration *)
type pvalue = 
  | PVString of string
  | PVRef of ident
  | PVInt of int 
  | PVEnum of ident * string
  | PVTuple of pvalue list
  | PVList of pvalue list
  | PVTyped of pvalue * ptype

(* contract field: ident * type * initial value *)
type contract_field = ident * ptype * pvalue [@@deriving show {with_path = false}]

(* contract entry: ident * params * commands *)
type contract_entry = ident * (ident * ptype) list * unit [@@deriving show {with_path = false}]

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
  | DInterface of ident * ident option * (signature list)

  (* identifier * extends * implements * entrypoints *)
  | DContract of ident * ident option * ident option * contract_field list * contract_entry list

  (* pure function, ident * params * rettype * body? *)
  | DFunction of ident * (ident * ptype) list * ptype * unit (* command list *)
[@@deriving show {with_path = false}]

(* a parse tree is a list of declarations; includes are unrolled by the parser *)
type t = declaration list [@@deriving show {with_path = false}]


let extract_contract pt contract = 
  let rec unroll_contract c cl = match c with 
  | (id, None, _, d, e) -> (d, e)
  | (id, Some(ex), _, d, e) -> 
    let (d', e') = unroll_contract (List.assoc ex cl) cl in
    (d@d', e@e')
  in
  let rec extract pt cl = match pt with 
    | [] -> []
    | (DConst (a,b,c))::pt' -> (DConst (a,b,c))::(extract pt' cl)
    | (DFunction (a,b,c,d))::pt' -> (DFunction (a,b,c,d))::(extract pt' cl)
    | (DContract (id,b,c,d,e))::pt' when id<>contract -> extract pt' @@ (id, (id,b,c,d,e))::cl
    | (DContract (id, None, _, d, e))::pt' -> [DContract (id, None, None, d, e)]
    | (DContract (id, Some(ex), _, d, e))::pt' -> 
      let (d, e) = unroll_contract (id, Some(ex), None, d, e) cl in
      [DContract (id, None, None, d, e)]
    | _::pt' -> extract pt' cl
  in extract pt []
;;