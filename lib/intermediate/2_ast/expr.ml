open Ttype

type expr = 
| ContractInstance of texpr 
| StorageEntry of iden
| BuildContractCodeAndStorage of iden * texpr list
| Entrypoint of texpr * iden

| TezosNow
| TezosAmount
| TezosBalance
| TezosChainId
| TezosSelf
| TezosSelfAddress
| TezosSetDelegate of texpr
| TezosSource
| TezosSender
| TezosAddressOfContract of texpr
| TezosContractOfAddress of texpr
| TezosTransfer of texpr * texpr * texpr
| TezosCreateContract of texpr * texpr * texpr
| TezosImplicitAccount of texpr

| CryptoBlake2B of texpr
| CryptoCheckSignature of texpr * texpr * texpr
| CryptoHashKey of texpr 
| CryptoSha256 of texpr
| CryptoSha512 of texpr

| Pack of texpr 
| Unpack of ttype * texpr

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
| Some of texpr
| Enum of ttype * string
| Typed of texpr * ttype
| List of texpr list 
| Set of texpr list 
| Map of (texpr * texpr) list
| BigMap of (texpr * texpr) list
| Tuple of texpr list
| Lambda of (iden * ttype) list * texpr
| Record of (iden * texpr) list
| EnumValue of (iden)
| RecordAccess of texpr * iden

(* option *)
| OptionGetSome of texpr 
| OptionIsNone of texpr
| OptionIsSome of texpr

(* map *)
| MapEmpty
| MapGetOpt of texpr * texpr
| MapGet of texpr * texpr * texpr
| MapMem of texpr * texpr
| MapSize of texpr
| MapMapWith of texpr * texpr
| MapFold of texpr * texpr * texpr
| MapUpdate of texpr * texpr * texpr 
| MapRemove of texpr * texpr 

(* bigmap *)
| BigMapEmpty
| BigMapGetOpt of texpr * texpr
| BigMapGet of texpr * texpr * texpr
| BigMapMem of texpr * texpr
| BigMapUpdate of texpr * texpr * texpr 
| BigMapRemove of texpr * texpr 

(* set *)
| SetEmpty
| SetMem of texpr * texpr
| SetSize of texpr
| SetUpdate of texpr * texpr * texpr 

(* list *)
| ListEmpty
| ListSize of texpr
| ListPrepend of texpr * texpr
| ListMapWith of texpr * texpr
| ListHead of texpr
| ListTail of texpr
| ListFold of texpr * texpr * texpr

(* string *)
| StringConcat of texpr * texpr 
| StringSlice of texpr * texpr * texpr
| StringSize of texpr

(* tuple *)
| TupleFst of texpr
| TupleSnd of texpr

(* aritmetic *)
| Add of texpr * texpr
| Sub of texpr * texpr
| Mul of texpr * texpr
| Div of texpr * texpr
| Mod of texpr * texpr
| Abs of texpr
| ToInt of texpr
| Ediv of texpr * texpr
| Neg of texpr
| IsNat of texpr

(* bool *)
| And of texpr * texpr
| Or of texpr * texpr
| Not of texpr
| Lt of texpr * texpr
| Lte of texpr * texpr
| Gt of texpr * texpr
| Gte of texpr * texpr
| Eq of texpr * texpr
| Neq of texpr * texpr

| IfThenElse of texpr * texpr * texpr 
| MatchWith of texpr * (texpr * texpr) list
| CaseDefault
| Apply of texpr * texpr

| Fail of texpr
| FailIf of texpr
| FailIfMessage of texpr * texpr
| Assert of texpr    
     
| LetIn of iden * ttype * texpr * texpr
| Let of iden * ttype * texpr 
| LetTuple of (iden * ttype) list * texpr 
| LetTupleIn of (iden * ttype) list * texpr * texpr
| SAssign of iden * texpr
| SRecAssign of iden * iden * texpr 

| Seq of texpr * texpr

[@@deriving show {with_path = false}]

and texpr = (ttype * expr) [@@deriving show {with_path = false}]

