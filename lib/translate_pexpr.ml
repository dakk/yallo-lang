open Ast_ttype
open Ast_env
open Ast_expr
open Errors
open Translate_ptype

let show_ttype_got_expect t1 t2 = "got: '" ^ show_ttype t1 ^ "' expect '" ^ show_ttype t2 ^ "'"
let show_ttype_between_na t1 t2 = "between '" ^ show_ttype t1 ^ "' and '" ^ show_ttype t2 ^ "' is not allowed"
let show_ttype_not_cmp t1 t2 = "Types '" ^ show_ttype t1 ^ "' and '" ^ show_ttype t2 ^ "' are not comparable"

type iref = 
| Storage of ttype
| StorageEntry of ttype list
| Local of ttype
[@@deriving show {with_path = false}]

type bindings = (iden * iref) list
[@@deriving show {with_path = false}]

(* transform an pexpr to (ttype * expr) *)
let rec transform_expr (pe: Parse_tree.pexpr) (env': Env.t) (ic: bindings) : texpr = 
  let argv_to_list pel = match pel with | TTuple(tl) -> tl | _ -> [pel] in
  let transform_expr_list pel = List.map (fun p -> transform_expr p env' ic) pel in
  let transform_iexpr_list pel = List.map (fun (i, p) -> i, transform_expr p env' ic) pel in
  let transform_itype_list pel = List.map (fun (i, p) -> i, transform_type p env') pel in
  let push_ic i ii ic = (i, ii)::(List.remove_assoc i ic) in
  let push_local_many rl ic = List.fold_left (fun ic (i,x) -> (i, Local(x))::(List.remove_assoc i ic)) ic rl in
  let fold_container_type debs l =
    List.fold_left (fun acc xt -> if acc <> xt then 
      raise @@ TypeError (debs ^ " must have the same type: " ^ show_ttype acc ^ " <> " ^ show_ttype xt)
    else 
      xt
    ) (List.hd l) l
  in
  match pe with

  (* Option *)
  | PENone -> TOption (TAny), None
  | PESome (e) -> 
    let (tt, te) = transform_expr e env' ic in
    TOption (tt), Some (tt, te)

  (* Literals *)
  | PEUnit -> TUnit, Unit
  | PEString (s) -> TString, String (s)
  | PEAddress (s) -> TAddress, Address (s)
  | PEChainId (s) -> TChainId, ChainId (s)
  | PEBytes (s) -> TBytes, Bytes (Bytes.of_string s)
  | PEKey (s) -> TKey, Key (s)
  | PEKeyHash (s) -> TKeyHash, KeyHash (s)
  | PESignature (s) -> TSignature, Signature (s)
  | PENat (n) -> TNat, Nat (n)
  | PEInt (n) -> TInt, Int (n)
  | PEMutez (t) -> TMutez, Mutez (t)
  | PEBool (b) -> TBool, Bool (b)

  (* Composed types *)
  | PETuple (el) -> 
    let tel = el |> transform_expr_list in
    TTuple(fst @@ List.split tel), Tuple(tel)

  | PEList (el) -> 
    let ttres = el |> transform_expr_list in
    let (ttl, tel) = ttres |> List.split in
    if List.length ttl > 0 then 
      let lt = fold_container_type "List elements" ttl in TList(lt), List(ttres)
    else 
      TList(TAny), List([])

  | PEMap (el) -> 
    let l = List.map (fun (a, b) -> transform_expr a env' ic, transform_expr b env' ic) el in
    let keys, values = List.split l in

    (* get keys and values type *)
    let keyt = fold_container_type "Map keys" (fst @@ List.split keys) in 
    let valuet = fold_container_type "Map values" (fst @@ List.split values) in 

    TMap(keyt, valuet), Map (List.combine keys values)

  | PETyped (e, et) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let tt' = transform_type et env' in
    (match tt, tt', ee with 
    | TOption (TAny), TOption(t), None -> TOption(t), None
    | a, b, _ when a=b -> a, ee
    | a, b, c -> raise @@ TypeError ("Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c))

  | PELambda (argl, e) -> 
    let rl = argl |> transform_itype_list in
    let (tt, ee) = transform_expr e env' (push_local_many rl ic) in 
    let arg = (match List.length rl with 
      | 0 -> TUnit
      | 1 -> snd @@ List.hd rl
      | _ -> TTuple (snd @@ List.split rl)
    ) in
    TLambda (arg, tt), Lambda(rl, (tt, ee))

  | PERecord (l) -> 
    let l' = l |> transform_iexpr_list in 
    let (idtt, idee) = List.map (fun (i, (tt, ee)) -> (i, tt), (i, ee)) l' |> List.split in
    TRecord (idtt), Record (l')

    
  (* Enum value *)
  | PEHt (ii, i) -> 
    (match Env.get_type_opt ii env' with 
    | Some(TEnum (el)) -> 
      if List.find_opt (fun x -> x=i) el <> None then
        TEnum(el), EnumValue(i)
      else 
        raise @@ TypeError ("Enum value '" ^ i ^ "' not found in enum: " ^ show_ttype (TEnum(el)))
    | None -> raise @@ TypeError ("Unknown enum type '" ^ ii ^ "'")
    | _ -> raise @@ TypeError ("Accessor # is only usable on enum type"))


  (* PEApply(PETRef) tezos apis *)
  | PEApply (PETRef (i), el) ->
    let el' = el |> transform_expr_list in 
    (match i, el' with 
      | "sender", [] -> TAddress, TezosSender 
      | "source", [] -> TAddress, TezosSource
      | "chainId", [] -> TChainId, TezosChainId
      | "amount", [] -> TMutez, TezosAmount
      | "balance", [] -> TMutez, TezosBalance
      | "now", [] -> TTimestamp, TezosNow
      | "self", [] -> TContract (TUnit), TezosSelf
      | "selfAddress", [] -> TAddress, TezosSelfAddress
      | "address", [(TContract(ctt), ad)] -> TAddress, TezosAddressOfContract (TContract(ctt), ad)
      | "contract", [(TAddress, ad)] -> TContract(TAny), TezosContractOfAddress (TAddress, ad)
      | "setDelegate", [(TOption (TKeyHash), kho)] -> TOperation, TezosSetDelegate (TOption(TKeyHash), kho)
      | "setDelegate", [(TOption (TAny), None)] -> TOperation, TezosSetDelegate (TOption(TKeyHash), None)
      | "implicitAccount", [(TKeyHash, kh)] -> TContract (TUnit), TezosImplicitAccount(TKeyHash, kh)
      | "transfer", [(TAddress, a); (TMutez, am)] -> 
        TOperation, TezosTransfer(
          (TContract(TUnit), TezosContractOfAddress (TAddress, a)),
          (TUnit, Unit),
          (TMutez, am)
        )
      | "transfer", [(TContract(ct), c); (ct', cv); (TMutez, am)] when ct'=ct -> 
        TOperation, TezosTransfer ((TContract(ct), c), (ct', cv), (TMutez, am))
      | "createContract", [(TTuple([TContractCode; TContractStorage]), BuildContractCodeAndStorage(a,b)); (TOption (TKeyHash), kho); (TMutez, v)] -> 
        TTuple([TOperation; TAddress]), 
        TezosCreateContract((TAny, BuildContractCodeAndStorage(a, b)), (TOption (TKeyHash), kho), (TMutez, v))
      | "createContract", [(TTuple([TContractCode; TContractStorage]), BuildContractCodeAndStorage(a,b)); (TOption (TAny), None); (TMutez, v)] -> 
        TTuple([TOperation; TAddress]), 
        TezosCreateContract((TAny, BuildContractCodeAndStorage(a, b)), (TOption (TKeyHash), None), (TMutez, v))
      | _, _ -> 
        List.iter (fun (t,e) -> (show_expr e ^ " : " ^ show_ttype t) |> print_endline) el';
        raise @@ APIError("Invalid call to Tezos." ^ i)
    )

  (* PEApply(PECRef) crypto apis *)
  | PEApply (PECRef (i), el) -> 
    let el' = el |> transform_expr_list in 
    (match i, el' with 
      | "blake2b", [(TBytes, e)] -> TBytes, CryptoBlake2B(TBytes, e)
      | "hashKey", [(TKey, e)] -> TKeyHash, CryptoHashKey(TBytes, e)
      | "sha256", [(TBytes, e)] -> TBytes, CryptoSha256(TBytes, e)
      | "sha512", [(TBytes, e)] -> TBytes, CryptoSha512(TBytes, e)
      | "checkSignature", [(TKey, ek); (TSignature, es); (TBytes, ed)] ->
        TBool, CryptoCheckSignature ((TKey, ek), (TSignature, es), (TBytes, ed))
      | _, _ -> raise @@ APIError ("Invalid call to Crypto." ^ i)
    )

  (* PEDot on base *)
  | PEApply (PEDot (PERef("Set"), "empty"), []) -> TSet(TAny), SetEmpty
  | PEApply (PEDot (PERef("List"), "empty"), []) -> TList(TAny), ListEmpty
  | PEApply (PEDot (PERef("Map"), "empty"), []) -> TMap(TAny, TAny), MapEmpty
  | PEApply (PEDot (PERef("BigMap"), "empty"), []) -> TBigMap(TAny, TAny), BigMapEmpty

  (* PEApply(PEDot) base type apis *)
  | PEApply (PEDot(e,i), el) -> 
    let (te, ee) = transform_expr e env' ic in
    let el' = el |> transform_expr_list in 
    (match te, i, el' with 
      (* List *)
      | TList (_), "size", [] -> TNat, ListSize (te, ee)
      | TList (l), "head", [] -> l, ListHead (te, ee)
      | TList (l), "tail", [] -> TList (l), ListTail (te, ee)
      | TList (l), "prepend", [(ll, e)] when ll = l -> TList (l), ListPrepend ((te, ee), (ll, e))
      | TList (l), "mapWith", [(TLambda (ll, rt), lame)] when l = ll -> TList (rt), ListMapWith ((te, ee), (TLambda (ll, rt), lame))
      | TList (l), "fold", [(TLambda (TTuple([ll; rt']), rt), lame); (ft, ff)] when l=ll && rt=rt' && rt=ft -> 
        ft, ListFold((te, ee), (TLambda (TTuple([ll; rt']), rt), lame), (ft, ff))

      (* Map *)
      | TMap (_, _), "size", [] -> TNat, MapSize (te, ee)
      | TMap (kt, kv), "get", [(kk, e)] when kk = kt -> TOption (kv), MapGetOpt((te, ee), (kk, e))
      | TMap (kt, kv), "get", [(kk, e); (kvv, kvd)] when kvv=kv && kk = kt -> 
        kv, MapGet((te, ee), (kk,e), (kvv,kvd))
      | TMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, MapMem((te,ee), (kk, e))
      | TMap (kt, _), "remove", [(kk, e)] when kk = kt -> TUnit, MapRemove((te, ee), (kk, e))
      | TMap (kt, kv), "mapWith", [(TLambda (TTuple([a;b]), rt), lame)] when (a=kt && b=kv) -> 
        TMap (kt, rt), MapMapWith ((te, ee), (TLambda (TTuple([a;b]), rt), lame))
      | TMap (kt, kv), "fold", [(TLambda (TTuple([llkt; llkv; rt']), rt), lame); (ft, ff)] when kt=llkt && kv=llkv && rt=rt' && rt=ft -> 
        ft, MapFold((te, ee), (TLambda (TTuple([llkt; llkv; rt']), rt), lame), (ft, ff))
      | TMap (kt, kv), "update", [(kkt, ek); (kkv, ev)] when kkt=kt && kkv=kv ->
        TUnit, MapUpdate((te, ee), (kkt, ek), (kkv, ev))

      (* BigMap *)
      | TBigMap (kt, kv), "get", [(kk, e)] when kk = kt -> TOption(kv), BigMapGetOpt((te, ee), (kk, e))
      | TBigMap (kt, kv), "get", [(kk, e); (kvv, kvd)] when kvv=kv && kk = 
        kt -> kv, BigMapGet((te, ee), (kk,e), (kvv,kvd))
      | TBigMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, BigMapMem((te,ee), (kk, e))
      | TBigMap (kt, _), "remove", [(kk, e)] when kk = kt -> TUnit, BigMapRemove((te, ee), (kk, e))
      | TBigMap (kt, kv), "update", [(kkt, ek); (kkv, ev)] when kkt=kt && kkv=kv ->
        TUnit, BigMapUpdate((te, ee), (kkt, ek), (kkv, ev))

      (* Set *)
      | TSet (_), "size", [] -> TNat, SetSize (te, ee)
      | TSet (kt), "mem", [(ll, e)] when kt = ll -> TBool, SetMem ((te, ee), (ll, e))
      | TSet (kt), "update", [(kkt, ek); (TBool, ev)] when kkt=kt ->
        TUnit, SetUpdate((te, ee), (kkt, ek), (TBool, ev))

      (* String *)
      | TString, "slice", [(TInt, i1); (TInt, i2)] -> TString, StringSlice ((te, ee), (TInt, i1), (TInt, i2))
      | TString, "size", [] -> TNat, StringSize(te, ee)

      (* Tuple *)
      | TTuple ([a; _]), "fst", [] -> a, TupleFst (te, ee)
      | TTuple ([_; b]), "snd", [] -> b, TupleSnd (te, ee)

      (* Interface to contract instance *)
      | TInterface(sl), "of", [(TAddress, ta)] -> TContractInstance(TInterface(sl)), ContractInstance(TAddress, ta)

      (* contract instance call *)
      | TContractInstance(TInterface(sl)), i, tl -> 
        (match List.assoc_opt i sl with 
        | None -> raise @@ ContractError ("Unknown contract entrypoint '" ^ i ^ "' on contract instance")
        | Some(es) when es=(fst @@ List.split tl) -> TOperation, (
          match List.length tl with 
          | 0 -> Apply((TContract(TTuple (es)), Entrypoint((te, ee), i)), (TUnit, Unit))
          | 1 -> Apply((TContract(TTuple (es)), Entrypoint((te, ee), i)), List.hd @@ tl)
          | _ -> Apply((TContract(TTuple (es)), Entrypoint((te, ee), i)), (TTuple(fst @@ List.split tl), Tuple(tl)))
        )
        | _ -> raise @@ TypeError ("Invalid types on contract instance apply over entrypoint '" ^ i ^ "'")
      )

      | _, i, _-> 
        raise @@ TypeError ("Invalid apply of " ^ i ^ " over '" ^ show_ttype te ^ "'")
    )

  (* PEDot *)
  | PEDot (e, i) -> 
    let (te, ee) = transform_expr e env' ic in
    (match te with 
    (* PEDot entrypoint access *)
    | TContractInstance(TInterface(ct)) -> 
      (match List.assoc_opt i ct with 
        | None -> raise @@ ContractError ("Unkown contract entrypoint '" ^ i ^ "'")
        | Some(tl) when List.length tl > 1 -> 
          TContract(TTuple(tl)), Entrypoint((te, ee), i)
        | Some(tl) when List.length tl = 1 -> 
          TContract(List.hd tl), Entrypoint((te, ee), i)
        | Some(_) -> 
          TContract(TUnit), Entrypoint((te, ee), i))

    (* PEDot record access *)
    | TRecord(t) -> 
      (match List.assoc_opt i t with 
        | None -> raise @@ TypeError ("Unkown record field '" ^ i ^ "'")
        | Some(t) -> t, RecordAccess((te, ee), i))
    | _ -> raise @@ InvalidExpression ("Unhandled dot access of '" ^ i ^ "' on expression '" ^ show_expr ee ^ "'")
    )



  (* Arithmetic *)
  | PEAdd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TTimestamp, TInt -> TTimestamp
      | TInt, TTimestamp -> TTimestamp
      | TMutez, TMutez -> TMutez
      | TString, TString -> TString 
      | _, _ -> raise @@ TypeError ("Add " ^ show_ttype_between_na tt1 tt2)
    ) in
    if tt1 = TString then rt, StringConcat ((tt1, ee1), (tt2, ee2)) else rt, Add ((tt1, ee1), (tt2, ee2))

  | PEMul (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TMutez, TNat -> TMutez
      | TNat, TMutez -> TMutez
      | _, _ -> raise @@ TypeError ("Mul " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Mul ((tt1, ee1), (tt2, ee2))

  | PEDiv (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TOption(TTuple([TNat; TNat]))
      | TNat, TInt -> TOption(TTuple([TInt; TNat])) 
      | TInt, TNat -> TOption(TTuple([TInt; TNat])) 
      | TInt, TInt -> TOption(TTuple([TInt; TNat])) 
      | TMutez, TNat -> TOption(TTuple([TMutez; TMutez])) 
      | TMutez, TMutez -> TOption(TTuple([TMutez; TNat])) 
      | _, _ -> raise @@ TypeError ("EDiv " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Div ((tt1, ee1), (tt2, ee2))

  | PESub (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    let rt = (match tt1, tt2 with 
      | TNat, TNat -> TNat
      | TNat, TInt -> TInt 
      | TInt, TNat -> TInt
      | TInt, TInt -> TInt
      | TTimestamp, TInt -> TTimestamp
      | TTimestamp, TTimestamp -> TInt
      | TMutez, TMutez -> TMutez
      | _, _ -> raise @@ TypeError ("Sub " ^ show_ttype_between_na tt1 tt2)
    ) in
    rt, Sub ((tt1, ee1), (tt2, ee2))

  (* Boolean *)
  | PENot (e1) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    if tt1 = TBool then TBool, Not (tt1, ee1) 
    else raise @@ TypeError ("Not needs a boolean expression, got: '" ^ show_ttype tt1 ^ "'")

  | PEOr (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, Or ((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError ("Or branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'"))

  | PEAnd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, And ((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError ("And branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'"))
  
  (* Compare *)
  | PEGt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gt((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))

  | PEGte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gte((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))
    
  | PELt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lt((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))

  | PELte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lte((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))

    | PEEq (e1, e2) -> 
      let (tt1, ee1) = transform_expr e1 env' ic in 
      let (tt2, ee2) = transform_expr e2 env' ic in 
      (match (attributes tt1).cmp, (attributes tt2).cmp with 
      | true, true -> TBool, Eq((tt1, ee1), (tt2, ee2))
      | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))

  | PENeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Neq((tt1, ee1), (tt2, ee2))
    | _, _ -> raise @@ TypeError (show_ttype_not_cmp tt1 tt2))
      

  (* symbol reference *)
  | PESRef (i) ->
    (match List.assoc_opt i ic with 
      | None -> raise @@ ContractError ("Unknow storage field: '" ^ i ^ "'")
      | Some (Storage(t)) -> t, StorageRef (i)
      | Some (StorageEntry(tl)) when List.length tl = 0 -> TContract(TUnit), StorageEntry (i)
      | Some (StorageEntry(tl)) when List.length tl = 1 -> TContract(List.hd tl), StorageEntry (i)
      | Some (StorageEntry(tl)) when List.length tl > 1 -> TContract(TTuple(tl)), StorageEntry (i)
      | _ -> raise @@ ContractError ("Symbol '" ^ i ^ "' is not a storage field")
      )
  
  | PERef (i) -> 
    (match List.assoc_opt i ic with 
    | None -> Env.get_ref i env', GlobalRef (i)
    | Some (Local(t)) -> t, LocalRef (i)
    | _ -> raise @@ SymbolNotFound ("Symbol '" ^ i ^ "' is not a valid ref")
    )

  (* apply *)
  | PEApply (PERef("assert"), c) ->
    if List.length c <> 1 then raise @@ APIError ("Assert need only one argument");
    let tt, ee = transform_expr (List.hd c) env' ic in
    if tt <> TBool then raise @@ TypeError ("Assert need a bool expression, got: " ^ show_ttype tt);
    TUnit, Assert(tt, ee)

  | PEApply (PERef("fail"), c) ->
    if List.length c <> 1 then raise @@ APIError ("Fail need only one argument");
    let tt, ee = transform_expr (List.hd c) env' ic in
    if tt <> TString then raise @@ TypeError ("Fail need a string expression, got: " ^ show_ttype tt);
    TUnit, Fail(tt, ee)

  | PEApply (PERef("failif"), c) ->
    ( match c |> transform_expr_list with 
    | [(TBool, co); (TString, m)] -> TUnit, FailIfMessage((TBool, co), (TString, m))
    | [(TBool, co)] -> TUnit, FailIf((TBool, co))
    | _ -> raise @@ APIError ("Invalid arguments for failif");
    )


  | PEApply (e, el) -> 
    let (tt,ee) = transform_expr e env' ic in 
    (match tt with 
    (* Apply on lambda  *)
    | TLambda (arv, rettype) -> 
      let argl = argv_to_list arv in 
      let ap = el |> transform_expr_list in
      if List.length argl <> List.length ap then 
        raise @@ InvalidExpression ("Invalid argument number for lambda apply");
      if not @@ Ast_ttype.compare_list argl (fst @@ List.split ap) then 
        raise @@ TypeError ("Invalid argument types apply");
      if List.length argl = 1 then 
        rettype, Apply((tt, ee), (TTuple(argl), Tuple(ap)))
      else 
        rettype, Apply((tt, ee), List.hd ap)

    (* Apply on contract, it is a transfer without mutez *)
    | TContract (ttl) ->
      let (ptt, pee) = transform_expr (List.hd el) env' ic in 
      if ptt <> ttl then raise @@ InvalidExpression ("Invalid arguments for callback");
      TOperation, TezosTransfer((tt, ee), (ptt, pee), (TMutez, Mutez (0)))

    (* Apply on contract name, it is a buildcontractcodeandstorage, which is the argument of create_contract *)
    | TContractCode -> 
      (* TODO: check constructor parameters *)
      let cc = (match ee with | GlobalRef (c) -> c | _ -> raise @@ InvalidExpression ("Expected a globalref")) in
      TTuple([TContractCode; TContractStorage]), BuildContractCodeAndStorage (cc, el |> transform_expr_list)
      
    | _ -> raise @@ TypeError ("Applying on not a lambda: " ^ (Parse_tree.show_pexpr (PEApply(e, el))))
  )
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' ic in 
    let (te1, ee1) = transform_expr e1 env' ic in 
    let (te2, ee2) = transform_expr e2 env' ic in 
    (match tc, te1, te2 with 
    | TBool, TList(t), TList(t') when t <> t' && (t <> TAny || t' <> TAny) ->
      (if t <> TAny then TList(t) else TList(t')), IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when t = t' -> t, IfThenElse ((tc, ec), (te1, ee1), (te2, ee2))
    | TBool, t, t' when t <> t' -> 
      raise @@ TypeError ("If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'")
    | _, _, _ -> raise @@ TypeError ("If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'"))

  | PEMatchWith (e, bl) -> 
    let (te, ee) = transform_expr e env' ic in 
    let bl' = List.map (fun (cv, cex)  -> 
      let (tt, ee) = transform_expr cv env' ic in
      let (tcex, ecex) = transform_expr cex env' ic in
      if (tt <> te) && (tt <> TAny) then
        raise @@ TypeError ("Match case has an invalid value type; " ^ show_ttype_got_expect tt te)
      else ((tt, ee), tcex, (tcex, ecex)) 
    ) bl in
    (* assert that every branch as the same type *)
    let rett: ttype = List.fold_left (fun acc (_, tcex, _) -> 
      if acc <> tcex && tcex <> TUnit then  (* TODO: tany is only allowed for fail *)
        raise @@ TypeError ("Match branches should have same type; " ^ show_ttype_got_expect tcex acc)
      else if tcex = TUnit then acc else tcex
    ) (let (_,b,_) = List.hd bl' in b) bl'
    in rett, MatchWith ((te, ee), List.map (fun (a,_,c) -> (a,c)) bl')

  | PECaseDefault -> TAny, CaseDefault


  (* let-binding and sequences *)
  | PELetIn(i, Some(t), e, e1) -> 
    let t' = transform_type t env' in
    let (tt, ee) = transform_expr e env' ic in 
    if not @@ compare_type_lazy tt t' then raise @@ TypeError ("LetIn type mismatch; " ^ show_ttype_got_expect tt t');
    let (tt1, ee1) = transform_expr e1 env' @@ push_ic i (Local(t')) ic in 
    tt, LetIn (i, t', (tt, ee), (tt1, ee1))

  | PESeq(PELet(i, Some(t), e), en) -> 
    let t' = transform_type t env' in
    let (tt, ee) = transform_expr e env' ic in 
    if not @@ compare_type_lazy tt t' then raise @@ TypeError ("Let type mismatch; " ^ show_ttype_got_expect tt t'); 
    let (tnt, ene) = transform_expr en env' @@ push_ic i (Local(t')) ic in
    tnt, Seq((TUnit, Let(i, t', (tt, ee))), (tnt, ene))

  | PELetIn(i, None, e, e1) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let (tt1, ee1) = transform_expr e1 env' @@ push_ic i (Local(tt)) ic in 
    tt, LetIn (i, tt, (tt, ee), (tt1, ee1))

  | PELetTuple(tl, e) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> tt, LetTuple(List.combine ti tl', (tt, ee))
      | _ -> failwith "Expected a tuple"
    )

  | PELetTupleIn(tl, e, e1) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> 
        let tl' = List.combine ti tl' in 
        let (tt1, ee1) = transform_expr e1 env' @@ push_local_many tl' ic in 
        tt1, LetTupleIn(tl', (tt, ee), (tt1, ee1))
      | _ -> failwith "Expected a tuple"
    )

  | PESeq(PELetTuple(tl, e), en) -> 
    let (tt, ee) = transform_expr e env' ic in 
    (* TODO optional types of tl are ignored! *)
    let ti = fst @@ List.split tl in
    (match tt with 
      | TTuple(tl') -> 
        let tl' = List.combine ti tl' in 
        let (tnt, ene) = transform_expr en env' @@ push_local_many tl' ic in
        tnt, Seq((TUnit, LetTuple(tl', (tt, ee))), (tnt, ene))
      | _ -> failwith "Expected a tuple"
    )


  | PESeq(PELet(i, None, e), en) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let (tnt, ene) = transform_expr en env' @@ push_ic i (Local(tt)) ic in
    tnt, Seq((TUnit, Let(i, tt, (tt, ee))), (tnt, ene))

  | PESeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 <> TUnit then raise @@ InvalidExpression ("Cannot ignore non unit expression in sequence");
    (tt2, Seq((tt1, ee1), (tt2, ee2)))

  (* Storage assign *)
  | PEAssign (PESRef (x), e) -> 
    let (tte, eee) = transform_expr e env' ic in 
    let se = List.assoc x ic in 
    if Storage(tte) <> se then raise @@ InvalidExpression ("Invalid assignment: " ^ show_iref (Storage(tte)) ^ " " ^ show_iref se);
    TUnit, SAssign(x, (tte, eee))

  (* Storage record assign *)
  | PEAssign (PEDot(PESRef (x), i), e) -> 
    let (tte, eee) = transform_expr e env' ic in 
    (match List.assoc x ic with 
    | Storage(TRecord (tl)) ->
      let recel = List.assoc i tl in 
      if tte <> recel then raise @@ InvalidExpression ("Invalid assignment");
      TUnit, SRecAssign(x, i, (tte, eee))
    | _ -> raise @@ InvalidExpression ("Invalid assignment"))

  | ex -> raise @@ InvalidExpression ("Expression not handled yet: " ^ Parse_tree.show_pexpr ex)
