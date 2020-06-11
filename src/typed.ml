type iden = string
[@@deriving show {with_path = false}]

type ttype = 
  | TUnit
  | TAddress
  | TInt
  | TChainId
  | TNat
  | TMutez
  | TTimestamp
  | TBool
  | TSignature
  | TKeyHash
  | TKey
  | TString
  | TBytes
  | TLambda of ttype * ttype
  | TEnum of string list
  | TList of ttype
  | TSet of ttype
  | TMap of ttype * ttype 
  | TBigMap of ttype * ttype
  | TOption of ttype
  | TRecord of (iden * ttype) list
  | TTuple of ttype list 
  | TContract of ttype 
  | TCallback of ttype

type tattr = {
  push  : bool;
  cmp   : bool;
  pass  : bool;
  store : bool;
  pack  : bool;
  bm_val: bool;
} [@@deriving show {with_path = false}]

let attributes (t: ttype) = match t with 
  | TUnit ->          { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TAddress ->       { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TInt ->           { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TChainId ->       { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TNat ->           { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TMutez ->         { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TTimestamp ->     { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBool ->          { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TSignature ->     { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TKeyHash ->       { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TKey ->           { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TString ->        { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBytes ->         { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TLambda (_, _) -> { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TEnum (sl) ->     { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false } (* ? *)
  | TList (l) ->      { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TSet (l) ->       { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TMap (k,v) ->     { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBigMap (k,v) ->  { cmp=false; pass=true;  store=true;  push=false; pack=false; bm_val=false }
  | TOption (v) ->    { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TRecord (kl) ->   { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false } (* ? *)
  | TTuple (v) ->     { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TContract (v) ->  { cmp=false; pass=true;  store=false; push=false; pack=true;  bm_val=true  }
  | TCallback (v) ->  { cmp=false; pass=true;  store=false; push=false; pack=true;  bm_val=true  } (* ? *)


let rec show_ttype (t: ttype) = match t with 
| TUnit -> "unit"
| TAddress -> "address"
| TInt -> "int"
| TChainId -> "chain_id"
| TNat -> "nat"
| TMutez -> "mutez"
| TTimestamp -> "timestamp"
| TBool -> "bool"
| TSignature -> "signature"
| TKeyHash -> "key_hash"
| TKey -> "key"
| TString -> "string"
| TBytes -> "bytes"
| TLambda (p, r) -> show_ttype p ^ " -> " ^ show_ttype r
| TEnum (el) -> "enum (" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else ", ") ^ x) "" el ^ ")"
| TList (t) -> show_ttype t ^ " list"
| TSet (t) -> show_ttype t ^ " set"
| TMap (t, t') -> "(" ^ show_ttype t ^ ", " ^ show_ttype t' ^ ") map"
| TBigMap (t, t') -> "(" ^ show_ttype t ^ ", " ^ show_ttype t' ^ ") big_map"
| TOption (t) -> show_ttype t ^ " option"
| TRecord (l) -> "record { " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else ", ") ^ x ^ ": " ^ show_ttype xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ show_ttype x) "" tl ^ ")"
| TContract (t) -> show_ttype t ^ " contract"
| TCallback (t) -> show_ttype t ^ " callback"

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()

type expr = 
  | TezosSender
  | TezosNow
  | TezosAmount
  | TezosBalance
  | TezosChainId
  | TezosSelf
  | TezosCreateContract
  | TezosSetDelegate of expr
  | TezosSource
  | TezosTransfer
  | TezosAddressOfContract of expr
  | TezosContractOfAddress of expr

  | CryptoBlake2B of expr
  | CryptoCheckSignature
  | CryptoHashKey of expr 
  | CryptoSha256 of expr
  | CryptoSha512 of expr

  | LocalRef of iden 
  | StorageRef of iden

  | None
  | Unit 
  | Bool of bool
  | Nat of int 
  | Int of int 
  | Mutez of int
  | Address of string
  | String of string
  | Bytes of bytes
  | Some of expr
  | Enum of ttype * string
  | Typed of expr * ttype
  | List of expr list 
  | Set of expr list 
  | Map of (expr * expr) list
  | BigMap of (expr * expr) list
  | Tuple of expr list
  | Lambda of (iden * ttype) list * expr
  | Record of (iden * expr) list

  (* map *)
  | MapEmpty
  | MapGet of expr * expr
  | MapMem of expr * expr
  | MapSize of expr
  | MapUpdate of expr * expr * expr
  | MapRemove of expr * expr
  | MapMapWith of expr * expr

  (* bigmap *)
  | BigMapEmpty
  | BigMapGet of expr * expr
  | BigMapMem of expr * expr
  | BigMapUpdate of expr * expr * expr
  | BigMapRemove of expr * expr

  (* set *)
  | SetEmpty
  | SetMem of expr * expr
  | SetSize of expr
  | SetUpdate of expr * expr * expr

  (* list *)
  | ListEmpty
  | ListSize of expr
  | ListPrepend of expr * expr
  | ListMapWith of expr * expr
  | ListHead of expr
  | ListTail of expr

  (* string *)
  | StringConcat of expr * expr 
  | StringSlice of expr * expr * expr
  | StringSize of expr

  (* tuple *)
  | TupleFst of expr
  | TupleSnd of expr

  (* aritmetic *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Abs of expr
  | Ediv of expr * expr
  | Neg of expr
  | IsNat of expr

  (* bool *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Lte of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr

  | IfThenElse of expr * expr * expr 
  | MatchWith of expr * (expr * expr) list
  | Apply of expr * expr

  [@@deriving show {with_path = false}]

type texpr = (ttype * expr) [@@deriving show {with_path = false}]

type left_op = 
  | I of iden     (* i *)
  | S of iden     (* this.i *)
  [@@deriving show {with_path = false}]

type statement = 
  | FailIf of expr
  | FailIfMessage of expr * expr
  | Assert of expr           
  | MapUpdate of left_op * expr * expr 
  | BigMapUpdate of left_op * expr * expr 
  | SetUpdate of left_op * expr * expr 
  | MapRemove of left_op * expr 
  | BigMapRemove of left_op * expr 

  (* | CallBuiltin of iden * expr list   *)

  | ListIter of expr * expr
  | MapIter of expr * expr
  | DeclareVar of iden * ttype
  | AssignVar of left_op * expr
  | RecAssign of left_op * iden * expr
  | DeclareAssignVar of iden * ttype * expr
  | DeclareAssignVarTuple of (iden *  ttype) list * expr
  | Call of iden * expr list  
  | If of expr * statement list * (statement list) option          
  | Skip
  | Return of expr


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


type declaration =
  | Const of dconst
  | Interface of dinterface
  | Contract of dcontract
  | Function of dfunction
  (* [@@deriving show {with_path = false}] *)

type t = declaration list

type symbol_type = | Type | Interface | Const | Contract [@@deriving show {with_path = false}]

type env = {
  types: (iden * ttype) list;
  consts: (iden * texpr) list;
  symbols: (iden * symbol_type) list
} [@@deriving show {with_path = false}]

let start_env = {
  consts=[];
  types=[
    "unit", TUnit;
    "address", TAddress;
    "int", TInt;
    "chain_id", TChainId;
    "nat", TNat;
    "mutez", TMutez;
    "timestamp", TTimestamp;
    "bool", TBool;
    "signature", TSignature;
    "key_hash", TKeyHash;
    "key", TKey;
    "string", TString;
    "bytes", TBytes;
  ];
  symbols=[
    "unit", Type;
    "address", Type;
    "int", Type;
    "chain_id", Type;
    "nat", Type;
    "mutez", Type;
    "timestamp", Type;
    "bool", Type;
    "signature", Type;
    "key_hash", Type;
    "key", Type;
    "string", Type;
    "bytes", Type;
  ]
}

(* Fail if the symbol is already defined *)
let assert_symbol_absence (e: env) s = 
  match List.assoc_opt s e.symbols with 
  | None -> ()
  | Some (st) -> failwith ("Symbol '" ^ s ^ "' is already defined as " ^ show_symbol_type st)


(* transform a pttype to a ttype *)
let rec transform_type (pt: Parse_tree.ptype) (e: env): ttype = match pt with 
| Parse_tree.PTBuiltin (tn) -> 
  (match List.assoc_opt tn e.types with 
  | None -> failwith ("Undefined type '" ^ tn ^ "'")
  | Some (t) -> t)
| Parse_tree.PTTuple (tl) -> 
  TTuple (List.map (fun tt -> transform_type tt e) tl)
| Parse_tree.PTCont (c, tt) -> (
  let assert_cmp_key a = if not (attributes a).cmp then 
    failwith ("Type '" ^ show_ttype a ^ "' is not comparable and cannot be used as key of " ^ c)
    else () 
  in
  let tt' = transform_type tt e in
  match c with 
  | "list" -> TList (tt') 
  | "map" -> (match tt' with 
    | TTuple (a::b::[]) -> assert_cmp_key a; TMap (a, b)
    | _ -> failwith ("Type for map should be a tuple ('a, 'b'), got: " ^ show_ttype tt'))
  | "big_map" -> (match tt' with 
    | TTuple (a::b::[]) -> assert_cmp_key a; TBigMap (a, b)
    | _ -> failwith ("Type for big_map should be a tuple ('a', 'b'), got: " ^ show_ttype tt'))
  | "set" -> assert_cmp_key tt'; TSet (tt')
  | "option" -> TOption (tt')
  | "contract" -> TContract (tt')
  | "callback" -> TCallback (tt')
  | c -> failwith ("Invalid container type '" ^ c ^ "'")
)
| Parse_tree.PTRecord (el) -> TRecord (List.map (fun (n, tt) -> n, transform_type tt e) el)
| Parse_tree.PTEnum (e) -> TEnum (e)
| Parse_tree.PTLambda (p, r) -> TLambda (transform_type p e, transform_type r e)

(* transform an pexpr to (ttype * expr) *)
let transform_expr (pe: Parse_tree.pexpr) (e: env) : (ttype * expr) = (TUnit, Unit)


let rec extract (p: Parse_tree.t) (e: env): (env) = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (dt) :: p' -> 
    assert_symbol_absence e dt.id;

    extract p' { e with 
      symbols=(dt.id, Type)::e.symbols;
      types=(dt.id, transform_type dt.t e)::e.types;
    }

  (* global const *)
  | Parse_tree.DConst (dc) :: p' -> 
    assert_symbol_absence e dc.id;
    let et = transform_type dc.t e in
    let (t, exp) = transform_expr dc.v e in 

    if t <> et then 
      failwith ("Const '" ^ dc.id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found");

    extract p' { e with 
      symbols=(dc.id, Const)::e.symbols;
      consts=(dc.id, (t, exp))::e.consts;
    }



  (* functions *)

  (* interface *)

  (* contracts *)

  | _ :: p' -> extract p' e
  | [] -> e

(* in teoria dobbiamo estrarre tutto in un passaggio, altrimenti non siamo a conoscenza di cosa e' gia' definito o meno *) 
(* quando abbiamo un espressione, per esempio da assegnare ad una const, allora controlliamo se l'espressione
  ha lo stesso tipo della const *)
(* of_parse_tree returns the contract, with the env around it, so it needs also the contract to extract *)
(* creare un exception per ogni tipo di errore, catcharla nel compiler *)

let of_parse_tree (p: Parse_tree.t) = 
  let e = extract p start_env in 
  e |> show_env |> print_endline