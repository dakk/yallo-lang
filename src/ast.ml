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
  | TRecord of (string * ttype) list
  | TTuple of ttype list 
  | TContract of ttype 
  | TCallback of ttype
  [@@deriving show {with_path = false}]


type expr = 
  | FailIf of expr
  | FailIfMessage of expr * expr
  | Assert of expr

  | TezosSender
  | TezosNow
  | TezosAmount
  | TezosBalance
  | TezosChainId
  | TezosSelf
  | TezosAddressOfContract of expr
  | TezosContractOfAddress of expr
  | TezosCreateContract
  | TezosSetDelegate of expr
  | TezosSource
  | TezosTransfer
  | TezosBlake2B of expr
  | TezosCheckSignature
  | TezosHashKey of expr 
  | TezosSha256 of expr
  | TezosSha512 of expr

  | LocalRef of string 
  | StorageRef of string

  | None
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
  | Lambda of (string * ttype) list * expr
  | Record of (string * expr) list

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

type statement = 
  | ListIter of expr * expr
  | MapIter of expr * expr
  | Skip
  [@@deriving show {with_path = false}]

type declaration =
  | None
  [@@deriving show {with_path = false}]

type t = {
  a: int;
}




let of_parse_tree (p: Parse_tree.t) = {a=0}