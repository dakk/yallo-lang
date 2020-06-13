type iden = string
[@@deriving show {with_path = false}]

type ttype = 
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
  | TContractCode (* this is the reference to a contract code, used only on create_contract *)

type tattr = {
  push  : bool;
  cmp   : bool;
  pass  : bool;
  store : bool;
  pack  : bool;
  bm_val: bool;
} [@@deriving show {with_path = false}]

let attributes (t: ttype) = match t with 
  | TAny ->           failwith "TAny does not have attribute"
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
  | TEnum (_) ->      { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false } (* ? *)
  | TList (_) ->      { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TSet (_) ->       { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TMap (_,_) ->     { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TBigMap (_,_) ->  { cmp=false; pass=true;  store=true;  push=false; pack=false; bm_val=false }
  | TOption (_) ->    { cmp=false; pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TRecord (_) ->    { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false } (* ? *)
  | TTuple (_) ->     { cmp=true;  pass=true;  store=true;  push=true;  pack=true;  bm_val=true  }
  | TContract (_) ->  { cmp=false; pass=true;  store=false; push=false; pack=true;  bm_val=true  }
  | TContractCode ->  { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }
  | TOperation ->     { cmp=false; pass=false; store=false; push=false; pack=false; bm_val=false }


let rec show_ttype (at: ttype) = match at with 
| TAny -> "'a"
| TUnit -> "unit"
| TAddress -> "address"
| TInt -> "int"
| TChainId -> "chain_id"
| TOperation -> "operation"
| TNat -> "nat"
| TMutez -> "mutez"
| TTimestamp -> "timestamp"
| TBool -> "bool"
| TSignature -> "signature"
| TKeyHash -> "key_hash"
| TKey -> "key"
| TString -> "string"
| TBytes -> "bytes"
| TLambda (p, r) -> "(" ^ show_ttype p ^ " -> " ^ show_ttype r ^ ")"
| TEnum (el) -> "enum (" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else ", ") ^ x) "" el ^ ")"
| TList (t) -> show_ttype t ^ " list"
| TSet (t) -> show_ttype t ^ " set"
| TMap (t, t') -> "(" ^ show_ttype t ^ ", " ^ show_ttype t' ^ ") map"
| TBigMap (t, t') -> "(" ^ show_ttype t ^ ", " ^ show_ttype t' ^ ") big_map"
| TOption (t) -> show_ttype t ^ " option"
| TRecord (l) -> "record { " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else ", ") ^ x ^ ": " ^ show_ttype xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ show_ttype x) "" tl ^ ")"
| TContract (t) -> show_ttype t ^ " contract"
| TContractCode -> "contract_code"
| _ -> failwith ("Unhandled type")

let pp_ttype fmt (t: ttype) = Format.pp_print_string fmt (show_ttype t); ()