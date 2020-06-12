open Ast_ttype

type expr = 
| TezosSender
| TezosNow
| TezosAmount
| TezosBalance
| TezosChainId
| TezosSelf
| TezosSetDelegate of expr
| TezosSource
| TezosAddressOfContract of expr
| TezosContractOfAddress of expr
| TezosTransfer of expr * expr * expr
| TezosCreateContract of iden (* todef *)

| CryptoBlake2B of expr
| CryptoCheckSignature of expr * expr * expr
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
| KeyHash of string
| Key of string 
| Signature of string
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
| EnumValue of (iden)
| RecordAccess of expr * iden

(* map *)
| MapEmpty
| MapGet of expr * expr
| MapMem of expr * expr
| MapSize of expr
| MapMapWith of expr * expr

(* bigmap *)
| BigMapEmpty
| BigMapGet of expr * expr
| BigMapMem of expr * expr

(* set *)
| SetEmpty
| SetMem of expr * expr
| SetSize of expr

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