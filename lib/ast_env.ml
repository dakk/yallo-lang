open Ast_ttype
open Ast_expr
open Errors

module Env = struct 
  type st = | Type | Interface | Const | Contract [@@deriving show {with_path = false}]
  type entry_sig = (iden * (iden * ttype) list)  [@@deriving show {with_path = false}]
  type ctor = (iden * ttype) list * (iden * expr) list [@@deriving show {with_path = false}]
  type entry = iden * (iden * ttype) list * expr [@@deriving show {with_path = false}]
  type contract = (ctor * entry list) [@@deriving show {with_path = false}]

  type t = {
    types:       (iden * ttype) list;
    consts:      (iden * texpr) list;
    contracts:   (iden * contract) list;
    ifaces:      (iden * entry_sig list) list;
    symbols:     (iden * st) list
  } [@@deriving show {with_path = false}]

  let start_env = {
    ifaces=[];
    contracts=[];
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
      "operation", TOperation
    ];
    symbols=[
      "operation", Type;
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
  let assert_symbol_absence (e: t) s = 
    match List.assoc_opt s e.symbols with 
    | None -> ()
    | Some (st) -> raise @@ DuplicateSymbolError ("Symbol '" ^ s ^ "' is already defined as " ^ show_st st)


  let get_type_opt tn (e: t) = List.assoc_opt tn e.types

  let get_ref sn (e: t) = 
    match List.assoc_opt sn e.symbols with 
    | None -> raise @@ SymbolNotFound("Unknown reference to symbol '" ^ sn ^ "'")
    | Some (Const) -> let (tt, _) = List.assoc sn e.consts in tt     
    | Some (Contract) -> TContractCode  
    | Some (Interface) -> 
      let esl = List.assoc sn e.ifaces in 
      let esl' = List.map (fun (i, pl) -> i, List.map (fun (_, pt) -> pt) pl) esl in 
      TInterface (esl')
    | _ -> raise @@ SymbolNotFound("Symbol '" ^ sn ^ "' not found in env")
end