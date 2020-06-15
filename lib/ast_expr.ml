open Ast_ttype

type expr = 
| ContractInstance of expr 
| StorageEntry of iden
| BuildContractCodeAndStorage of iden * expr list
| Entrypoint of expr * iden

| TezosNow
| TezosAmount
| TezosBalance
| TezosChainId
| TezosSelf
| TezosSetDelegate of expr
| TezosSource
| TezosSender
| TezosAddressOfContract of expr
(* | TezosContractOfAddress of expr we have to define contract signature to extract *)
| TezosTransfer of expr * expr * expr
| TezosCreateContract of expr * expr * expr
| TezosImplicitAccount of expr

| CryptoBlake2B of expr
| CryptoCheckSignature of expr * expr * expr
| CryptoHashKey of expr 
| CryptoSha256 of expr
| CryptoSha512 of expr

| LocalRef of iden 
| StorageRef of iden
| GlobalRef of iden

| None
| Unit 
| Bool of bool
| Nat of int 
| Int of int 
| Mutez of int
| ChainId of int
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
| MapGetOpt of expr * expr
| MapGet of expr * expr * expr
| MapMem of expr * expr
| MapSize of expr
| MapMapWith of expr * expr
| MapFold of expr * expr * expr
| MapUpdate of expr * expr * expr 
| MapRemove of expr * expr 

(* bigmap *)
| BigMapEmpty
| BigMapGetOpt of expr * expr
| BigMapGet of expr * expr * expr
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
| ListFold of expr * expr * expr

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

| Fail of expr
| FailIf of expr
| FailIfMessage of expr * expr
| Assert of expr    
     
| LetIn of iden * ttype * expr * expr
| Let of iden * ttype * expr 
| LetTuple of (iden * ttype) list * expr 
| LetTupleIn of (iden * ttype) list * expr * expr
| SAssign of iden * expr
| SRecAssign of iden * iden * expr 

| Seq of expr * expr

[@@deriving show {with_path = false}]



type texpr = (ttype * expr) [@@deriving show {with_path = false}]