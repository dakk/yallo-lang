type iden = string
[@@deriving show {with_path = false}]

type btype = 
  | TAnnot of btype * iden
  | TAny
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
  | TOperation
  | TLambda of btype * btype
  | TList of btype
  | TSet of btype
  | TMap of btype * btype 
  | TBigMap of btype * btype
  | TOption of btype
  | TPair of btype * btype
  | TOr of btype * btype
  | TContract of btype 
