open Ast_ttype
open Ast_expr
open Ast_statement


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

(* transform a pttype to a ttype *)
let rec transform_type (pt: Parse_tree.ptype) (e: Env.t): ttype = match pt with 
| Parse_tree.PTBuiltin (tn) -> 
  (match Env.get_type_opt tn e with 
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


(* let api = [
  (PEDot ((PERef "Map"), "empty"), MapEmpty)
]; *)

(* transform an pexpr to (ttype * expr) *)
let rec transform_expr (pe: Parse_tree.pexpr) (env': Env.t) : (ttype * expr) = 
  let fold_container_type debs l =
    List.fold_left (fun acc xt -> if acc <> xt then 
      failwith @@ debs ^ " must have the same type: " ^ show_ttype acc ^ " <> " ^ show_ttype xt
    else 
      xt
    ) (List.hd l) l
  in
  match pe with
  | PEUnit -> TUnit, Unit

  | PEHt (ii, i) -> 
    (match Env.get_type_opt ii env' with 
    | Some(TEnum (el)) -> 
      if List.find_opt (fun x -> x=i) el <> None then
        TEnum(el), EnumValue(i)
      else 
        failwith @@ "Enum value '" ^ i ^ "' not found in enum: " ^ show_ttype (TEnum(el))
    | None -> failwith @@ "Unknown enum type '" ^ ii ^ "'"
    | _ -> failwith "Accessor # is only usable on enum type")

  (* PEDot on base *)
  | PEDot (PERef("Set"), "empty") -> TSet(TAny), SetEmpty
  | PEDot (PERef("List"), "empty") -> TList(TAny), ListEmpty
  | PEDot (PERef("Map"), "empty") -> TMap(TAny, TAny), MapEmpty
  | PEDot (PERef("BigMap"), "empty") -> TBigMap(TAny, TAny), BigMapEmpty

  (* PEApply(PEDot) base type apis *)
  | PEApply (PEDot(e,i), el) -> 
    let (te, ee) = transform_expr e env' in
    let el' = List.map (fun a -> transform_expr a env') el in 
    (match te, i, el' with 
      (* List *)
      | TList (l), "size", [] -> TNat, ListSize (ee)
      | TList (l), "head", [] -> l, ListHead (ee)
      | TList (l), "tail", [] -> TList (l), ListTail (ee)
      | TList (l), "prepend", [(ll, e)] when ll = l -> TList (l), ListPrepend (ee, e)
      | TList (l), "mapWith", [(TLambda (ll, rt), lame)] when l = ll -> TList (rt), ListMapWith (ee, lame)

      (* Map *)
      | TMap (kt, kv), "size", [] -> TNat, MapSize (ee)
      | TMap (kt, kv), "get", [(kk, e)] when kk = kt -> kv, MapGet(ee, e)
      | TMap (kt, kv), "mem", [(kk, e)] when kk = kt -> TBool, MapMem(ee, e)
      | TMap (kt, kv), "mapWith", [(TLambda (TTuple([a;b]), rt), lame)] when (a=kt && b=kv) -> 
        TMap (kt, rt), MapMapWith (ee, lame)

      (* BigMap *)
      | TBigMap (kt, kv), "get", [(kk, e)] when kk = kt -> kv, BigMapGet(ee, e)
      | TBigMap (kt, kv), "mem", [(kk, e)] when kk = kt -> TBool, BigMapMem(ee, e)

      (* Set *)
      | TSet (kt), "size", [] -> TNat, SetSize (ee)
      | TSet (kt), "mem", [(ll, e)] when kt = ll -> TBool, SetMem (ee, e)

      (* String *)
      | TString, "slice", [(TInt, i1); (TInt, i2)] -> TString, StringSlice (ee, i1, i2)
      | TString, "size", [] -> TNat, StringSize(ee)

      (* Tuple *)
      | TTuple ([a; b]), "fst", [] -> a, TupleFst (ee)
      | TTuple ([a; b]), "snd", [] -> b, TupleSnd (ee)

      | _, f, _-> 
        failwith @@ "Invalid apply of f over '" ^ show_ttype te ^ "'"
    )

  (* PEDot record access *)
  | PEDot (e, i) -> 
    let (te, ee) = transform_expr e env' in
    (match te with 
    | TRecord(t) -> 
      let f = List.assoc_opt i t in 
      (match List.assoc_opt i t with 
        | None -> failwith @@ "Unkown record field '" ^ i ^ "'"
        | Some(t) -> t, RecordAccess(ee, i))
    | _ -> failwith @@ "Unhandled pedot: " ^ Parse_tree.show_pexpr e ^ " " ^ i
    )

  (* Option *)
  | PENone -> TOption (TAny), None
  | PESome (e) -> 
    let (tt, te) = transform_expr e env' in
    TOption (tt), Some (te)

  (* Literals *)
  | PEString (s) -> TString, String (s)
  | PENat (n) -> TNat, Nat (n)
  | PEInt (n) -> TInt, Int (n)
  | PEMutez (t) -> TMutez, Mutez (t)
  | PEBool (b) -> TBool, Bool (b)

  (*  *)
  | PETuple (el) -> 
    let (ttl, tel) = List.map (fun x -> transform_expr x env') el |> List.split in
    TTuple(ttl), Tuple(tel)

  | PEList (el) -> 
    let (ttl, tel) = List.map (fun x -> transform_expr x env') el |> List.split in
    let lt = fold_container_type "List elements" ttl in 
    TList(lt), List(tel)

  | PEMap (el) -> 
    let l = List.map (fun (a, b) -> transform_expr a env', transform_expr b env') el in
    let keys, values = List.split l in

    (* get keys and values type *)
    let keyt = fold_container_type "Map keys" (fst @@ List.split keys) in 
    let valuet = fold_container_type "Map values" (fst @@ List.split values) in 

    TMap(keyt, valuet), Map (List.combine (snd @@ List.split keys) (snd @@ List.split values))

  | PETyped (e, et) -> 
    let (tt, ee) = transform_expr e env' in 
    let tt' = transform_type et env' in
    (match tt, tt', ee with 
    | TString, TAddress, String (a) -> TAddress, Address (a)
    | TOption (TAny), TOption(t), None -> TOption(t), None
    | a, b, c -> failwith @@ "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c)


  | PELambda (argl, e) -> 
    let rl = List.map (fun (i,t) -> i, transform_type t env') argl in
    let (tt, ee) = transform_expr e @@ Env.push_scope env' (Scope.of_params Lambda rl) in 
    let arg = (match List.length rl with 
      | 0 -> TUnit
      | 1 -> snd @@ List.hd rl
      | n -> TTuple (snd @@ List.split rl)
    ) in
    TLambda (arg, tt), Lambda(rl, ee)

  | PERecord (l) -> 
    let l' = List.map (fun (i,e) -> i, transform_expr e env') l in 
    let idtt = List.map (fun (i, (tt, ee)) -> i, tt) l' in
    let idee = List.map (fun (i, (tt, ee)) -> i, ee) l' in
    TRecord (idtt), Record (idee)


  (* Arithmetic *)
  | PEAdd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TTimestamp, TInt -> TTimestamp
      | TInt, TTimestamp -> TTimestamp
      | TMutez, TMutez -> TMutez
      | TString, TString -> TString 
      | a, b -> failwith @@ "Add between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    if tt1 = TString then rt, StringConcat (ee1, ee2) else rt, Add (ee1, ee2)

  | PEMul (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TMutez, TNat -> TMutez
      | TNat, TMutez -> TMutez
      | a, b -> failwith @@ "Mul between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    rt, Mul (ee1, ee2)

  | PESub (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TTimestamp, TInt -> TTimestamp
      | TTimestamp, TTimestamp -> TInt
      | TMutez, TMutez -> TMutez
      | a, b -> failwith @@ "Sub between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    rt, Sub (ee1, ee2)

  (* Boolean *)
  | PENot (e1) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    if tt1 = TBool then TBool, Not (ee1) 
    else failwith @@ "Not needs a boolean expression, got: '" ^ show_ttype tt1 ^ "'"

  | PEOr (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, Or (ee1, ee2)
    | _, _ -> failwith @@ "Or branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'")

  | PEAnd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, And (ee1, ee2)
    | _, _ -> failwith @@ "And branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'")
  
  (* Compare *)
  | PEGt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gt(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PEGte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gte(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")
    
  | PELt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lt(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PELte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lte(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

    | PEEq (e1, e2) -> 
      let (tt1, ee1) = transform_expr e1 env' in 
      let (tt2, ee2) = transform_expr e2 env' in 
      (match (attributes tt1).cmp, (attributes tt2).cmp with 
      | true, true -> TBool, Eq(ee1, ee2)
      | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PENeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' in 
    let (tt2, ee2) = transform_expr e2 env' in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Neq(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")
      

  | PERef (i) -> Env.get_ref i env', LocalRef (i)

  | PEApply (e, el) -> 
    let (tt,ee) = transform_expr e env' in 
    (match tt with 
    | TLambda (TTuple(argl), rettype) when (List.length argl) = (List.length el) -> 
      let ap = List.map (fun (arg, ex) -> 
        let (ptt, pee) = transform_expr ex env' in 
        if ptt <> arg then 
          failwith @@ "Invalid argument on apply: got '" ^ show_ttype ptt ^ "' expected '" ^ show_ttype arg ^ "'" 
        else pee
      ) @@ List.combine argl el 
      in rettype, Apply(ee, Tuple(ap))

    | TLambda (TTuple(argl), rettype) when (List.length argl) <> (List.length el) -> 
      failwith @@ "Invalid argument number for lambda apply"

    | TLambda (t, rettype) when (List.length el) = 1 -> 
      let (ptt, pee) = transform_expr (List.hd el) env' in 
      if ptt <> t then 
        failwith @@ "Invalid argument for apply, got '" ^ show_ttype ptt ^ "' expected '" ^ show_ttype t ^ "'"
      else
        rettype, Apply(ee, pee)

    | TLambda (t, rettype) when (List.length el) > 1 -> 
      failwith "This lambda expect only one argument"
      
    | _ -> failwith "Applying on not a labmda")
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' in 
    let (te1, ee1) = transform_expr e1 env' in 
    let (te2, ee2) = transform_expr e2 env' in 
    (match tc, te1, te2 with 
    | TBool, t, t' when t <> t' -> failwith @@ "If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'"
    | TBool, t, t' when t = t' -> t, IfThenElse (ec, ee1, ee2)
    | _, t, t' -> failwith @@ "If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'")

  | PEMatchWith (e, bl) -> 
    let (te, ee) = transform_expr e env' in 
    let bl' = List.map (fun (cv, cex)  -> 
      let (tt, ee) = transform_expr cv env' in
      let (tcex, ecex) = transform_expr cex env' in
      if (tt <> te) then
        failwith @@ "Match case has an invalid value type, got: '" ^ show_ttype tt ^ "' expect '" ^ show_ttype te ^ "'"
      else 
        (ee, tcex, ecex) 
    ) bl in
    (* assert that every branch as the same type *)
    let rett: ttype = List.fold_left (fun acc (ee, tcex, ecex) -> 
      if acc <> tcex then 
        failwith @@ "Match branches should have same type, got: '" ^ show_ttype tcex ^ "' expect '" ^ show_ttype acc ^ "'"
      else 
        tcex
    ) (let (a,b,c) = List.hd bl' in b) bl' 
    in rett, MatchWith (ee, List.map (fun (a,b,c) -> (a,c)) bl')

  | ex -> failwith @@ "expression not handled yet: " ^ Parse_tree.show_pexpr ex

  (*
  | PESRef of iden
  | PETRef of iden
  | PECRef of iden

  | PEDiv of pexpr * pexpr
  | PEMod of pexpr * pexpr

  | PEAnd of pexpr * pexpr
  | PEOr of pexpr * pexpr
  | PENot of pexpr

  | PEMatchWith of pexpr * (pexpr * pexpr) list
*)



let rec extract (p: Parse_tree.t) (e: Env.t): Env.t = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (dt) :: p' -> 
    Env.assert_symbol_absence e dt.id;

    extract p' { e with 
      symbols=(dt.id, Type)::e.symbols;
      types=(dt.id, transform_type dt.t e)::e.types;
    }

  (* global const *)
  | Parse_tree.DConst (dc) :: p' -> 
    Env.assert_symbol_absence e dc.id;
    let et = transform_type dc.t e in
    let (t, exp) = transform_expr dc.v e in 

    let t = match (t, et) with
      | TBigMap (TAny, TAny), TBigMap(a, b) -> et 
      | TMap (TAny, TAny), TMap(a, b) -> et 
      | TList (TAny), TList (a) -> et
      | TSet (TAny), TSet (a) -> et
      | TOption (TAny), TOption (a) -> et
      | TString, TAddress -> TAddress
      | a, b when a = b -> t
      | _, _ -> failwith ("Const '" ^ dc.id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
    in

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
  let e = extract p Env.start_env in 
  e |> Env.show |> print_endline