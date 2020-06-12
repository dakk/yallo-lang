open Ast_ttype
open Ast_expr

module Scope = struct
  type st = Const | Var [@@deriving show {with_path = false}]
  type sc = Lambda | Function | Entry | Ctor [@@deriving show {with_path = false}]

  type t = {
    stype: sc;
    consts: (iden * ttype) list;
    vars: (iden * ttype) list;
    symbols: (iden * st) list
  } [@@deriving show {with_path = false}]

  let empty s = { stype=s; consts=[]; vars=[]; symbols=[] }

  let of_params (ss: sc) (pl: (iden * ttype) list) = {
    stype= ss;
    consts= pl;
    vars= [];
    symbols= List.map (fun (i,t) -> (i, Const)) pl
  }

  let get_opt i s: (ttype option) = match List.assoc_opt i s.symbols with
  | None -> None
  | Some(Const) -> List.assoc_opt i s.consts
  | Some(Var) -> List.assoc_opt i s.vars

  let get i s: ttype = match get_opt i s with 
  | None -> raise Not_found
  | Some(v) -> v
end

module Env = struct 
  type st = | Type | Interface | Const | Contract [@@deriving show {with_path = false}]

  type t = {
    scope_stack: Scope.t list;
    types:       (iden * ttype) list;
    consts:      (iden * texpr) list;
    symbols:     (iden * st) list
  } [@@deriving show {with_path = false}]

  let start_env = {
    scope_stack=[];
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
  let assert_symbol_absence (e: t) s = 
    match List.assoc_opt s e.symbols with 
    | None -> ()
    | Some (st) -> failwith ("Symbol '" ^ s ^ "' is already defined as " ^ show_st st)

  let push_scope (e: t) (s: Scope.t) =
    { e with scope_stack=s::(e.scope_stack) }

  let pop_scope (e: t) = match e.scope_stack with 
    | [] -> failwith "empty scope!"
    | x::xl' -> { e with scope_stack=xl' }

  let get_type_opt tn (e: t) = List.assoc_opt tn e.types

  let get_ref sn (e: t) = 
    try 
      let s = List.hd e.scope_stack in 
      Scope.get sn s 
    with
    | _ -> 
      match List.assoc_opt sn e.symbols with 
      | None -> failwith @@ "Unknown reference to symbol '" ^ sn ^ "'"
      | Some (Const) -> let (tt, ee) = List.assoc sn e.consts in tt        
end