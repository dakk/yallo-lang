open Ast_ttype
open Ast_env
open Ast_expr
open Translate_ptype

(* transform an pexpr to (ttype * expr) *)
let rec transform_expr (pe: Parse_tree.pexpr) (env': Env.t) (ic: (iden * ttype) list) : (ttype * expr) = 
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

  (* PEApply(PETRef) tezos apis *)
  | PEApply (PETRef (i), el) ->
    let el' = List.map (fun a -> transform_expr a env' ic) el in 
    (match i, el' with 
      | "sender", [] -> TAddress, TezosSender 
      | "source", [] -> TAddress, TezosSource
      | "chainId", [] -> TChainId, TezosChainId
      | "amount", [] -> TMutez, TezosAmount
      | "balance", [] -> TMutez, TezosBalance
      | "now", [] -> TTimestamp, TezosNow
      | "setDelegate", [(TKeyHash, kh)] -> TOperation, TezosSetDelegate (kh)

(* | TezosSelf
| TezosImplicitAccount of iden (* todef *)
| TezosAddressOfContract of expr
| TezosContractOfAddress of expr
| TezosTransfer of expr * expr * expr
| TezosCreateContract of iden todef *)
      | _, _ -> failwith @@ "Invalid call to Tezos." ^ i
    )

  (* PEApply(PECRef) crypto apis *)
  | PEApply (PECRef (i), el) -> 
    let el' = List.map (fun a -> transform_expr a env' ic) el in 
    (match i, el' with 
      | "blake2b", [(TBytes, e)] -> TBytes, CryptoBlake2B(e)
      | "hashKey", [(TKey, e)] -> TKeyHash, CryptoHashKey(e)
      | "sha256", [(TBytes, e)] -> TBytes, CryptoSha256(e)
      | "sha512", [(TBytes, e)] -> TBytes, CryptoSha512(e)
      | "checkSignature", [(TKey, ek); (TSignature, es); (TBytes, ed)] ->
        TBool, CryptoCheckSignature (ek, es, ed)
      | _, _ -> failwith @@ "Invalid call to Crypto." ^ i
    )

  (* PEApply(PEDot) base type apis *)
  | PEApply (PEDot(e,i), el) -> 
    let (te, ee) = transform_expr e env' ic in
    let el' = List.map (fun a -> transform_expr a env' ic) el in 
    (match te, i, el' with 
      (* List *)
      | TList (_), "size", [] -> TNat, ListSize (ee)
      | TList (l), "head", [] -> l, ListHead (ee)
      | TList (l), "tail", [] -> TList (l), ListTail (ee)
      | TList (l), "prepend", [(ll, e)] when ll = l -> TList (l), ListPrepend (ee, e)
      | TList (l), "mapWith", [(TLambda (ll, rt), lame)] when l = ll -> TList (rt), ListMapWith (ee, lame)
      | TList (l), "fold", [(TLambda (TTuple([ll; rt']), rt), lame); (ft, ff)] when l=ll && rt=rt' && rt=ft -> 
        ft, ListFold(ee, lame, ff)

      (* Map *)
      | TMap (_, _), "size", [] -> TNat, MapSize (ee)
      | TMap (kt, kv), "get", [(kk, e)] when kk = kt -> TOption (kv), MapGetOpt(ee, e)
      | TMap (kt, kv), "get", [(kk, e); (kvv, kvd)] when kvv=kv && kk = kt -> kv, MapGet(ee, e, kvd)
      | TMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, MapMem(ee, e)
      | TMap (kt, _), "remove", [(kk, e)] when kk = kt -> TUnit, MapRemove(ee, e)
      | TMap (kt, kv), "mapWith", [(TLambda (TTuple([a;b]), rt), lame)] when (a=kt && b=kv) -> 
        TMap (kt, rt), MapMapWith (ee, lame)
      | TMap (kt, kv), "fold", [(TLambda (TTuple([llkt; llkv; rt']), rt), lame); (ft, ff)] when kt=llkt && kv=llkv && rt=rt' && rt=ft -> 
        ft, MapFold(ee, lame, ff)
      | TMap (kt, kv), "update", [(kkt, ek); (kkv, ev)] when kkt=kt && kkv=kv ->
        TUnit, MapUpdate(ee, ek, ev)

      (* BigMap *)
      | TBigMap (kt, kv), "get", [(kk, e)] when kk = kt -> TOption(kv), BigMapGetOpt(ee, e)
      | TBigMap (kt, kv), "get", [(kk, e); (kvv, kvd)] when kv=kvv && kk = kt -> kv, BigMapGet(ee, e, kvd)
      | TBigMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, BigMapMem(ee, e)
      | TBigMap (kt, _), "remove", [(kk, e)] when kk = kt -> TUnit, BigMapRemove(ee, e)
      | TBigMap (kt, kv), "update", [(kkt, ek); (kkv, ev)] when kkt=kt && kkv=kv ->
        TUnit, BigMapUpdate(ee, ek, ev)

      (* Set *)
      | TSet (_), "size", [] -> TNat, SetSize (ee)
      | TSet (kt), "mem", [(ll, e)] when kt = ll -> TBool, SetMem (ee, e)
      | TSet (kt), "update", [(kkt, ek); (TBool, ev)] when kkt=kt ->
        TUnit, SetUpdate(ee, ek, ev)

      (* String *)
      | TString, "slice", [(TInt, i1); (TInt, i2)] -> TString, StringSlice (ee, i1, i2)
      | TString, "size", [] -> TNat, StringSize(ee)

      (* Tuple *)
      | TTuple ([a; _]), "fst", [] -> a, TupleFst (ee)
      | TTuple ([_; b]), "snd", [] -> b, TupleSnd (ee)

      | _, _, _-> 
        failwith @@ "Invalid apply of f over '" ^ show_ttype te ^ "'"
    )

  (* PEDot record access *)
  | PEDot (e, i) -> 
    let (te, ee) = transform_expr e env' ic in
    (match te with 
    | TRecord(t) -> 
      (match List.assoc_opt i t with 
        | None -> failwith @@ "Unkown record field '" ^ i ^ "'"
        | Some(t) -> t, RecordAccess(ee, i))
    | _ -> failwith @@ "Unhandled pedot: " ^ Parse_tree.show_pexpr e ^ " " ^ i
    )

  (* Option *)
  | PENone -> TOption (TAny), None
  | PESome (e) -> 
    let (tt, te) = transform_expr e env' ic in
    TOption (tt), Some (te)

  (* Literals *)
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

  (*  *)
  | PETuple (el) -> 
    let (ttl, tel) = List.map (fun x -> transform_expr x env' ic) el |> List.split in
    TTuple(ttl), Tuple(tel)

  | PEList (el) -> 
    let (ttl, tel) = List.map (fun x -> transform_expr x env' ic) el |> List.split in
    if List.length ttl > 0 then 
      let lt = fold_container_type "List elements" ttl in TList(lt), List(tel)
    else 
      TList(TAny), List([])

  | PEMap (el) -> 
    let l = List.map (fun (a, b) -> transform_expr a env' ic, transform_expr b env' ic) el in
    let keys, values = List.split l in

    (* get keys and values type *)
    let keyt = fold_container_type "Map keys" (fst @@ List.split keys) in 
    let valuet = fold_container_type "Map values" (fst @@ List.split values) in 

    TMap(keyt, valuet), Map (List.combine (snd @@ List.split keys) (snd @@ List.split values))

  | PETyped (e, et) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let tt' = transform_type et env' in
    (match tt, tt', ee with 
    (* | TString, TKeyHash, String (a) -> TKeyHash, KeyHash (a)
    | TString, TKey, String (a) -> TKey, Key (a)
    | TString, TSignature, String (a) -> TSignature, Signature (a)
    | TString, TAddress, String (a) -> TAddress, Address (a)
    | TString, TBytes, String (a) -> TBytes, Bytes (Bytes.of_string a)
    | TBytes, TString, Bytes (a) -> TString, String (Bytes.to_string a) *)
    | TOption (TAny), TOption(t), None -> TOption(t), None
    | a, b, _ when a=b -> a, ee
    | a, b, c -> failwith @@ "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c)


  | PELambda (argl, e) -> 
    let rl = List.map (fun (i,t) -> i, transform_type t env') argl in
    let (tt, ee) = transform_expr e env' (ic @ rl) in 
    let arg = (match List.length rl with 
      | 0 -> TUnit
      | 1 -> snd @@ List.hd rl
      | _ -> TTuple (snd @@ List.split rl)
    ) in
    TLambda (arg, tt), Lambda(rl, ee)

  | PERecord (l) -> 
    let l' = List.map (fun (i,e) -> i, transform_expr e env' ic) l in 
    let (idtt, idee) = List.map (fun (i, (tt, ee)) -> (i, tt), (i, ee)) l' |> List.split in
    TRecord (idtt), Record (idee)


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
      | _, _ -> failwith @@ "Add between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    if tt1 = TString then rt, StringConcat (ee1, ee2) else rt, Add (ee1, ee2)

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
      | _, _ -> failwith @@ "Mul between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    rt, Mul (ee1, ee2)

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
      | _, _ -> failwith @@ "Sub between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
    ) in
    rt, Sub (ee1, ee2)

  (* Boolean *)
  | PENot (e1) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    if tt1 = TBool then TBool, Not (ee1) 
    else failwith @@ "Not needs a boolean expression, got: '" ^ show_ttype tt1 ^ "'"

  | PEOr (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, Or (ee1, ee2)
    | _, _ -> failwith @@ "Or branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'")

  | PEAnd (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match tt1, tt2 with 
    | TBool, TBool -> TBool, And (ee1, ee2)
    | _, _ -> failwith @@ "And branches should be boolean expressions, got: '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "'")
  
  (* Compare *)
  | PEGt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gt(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PEGte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Gte(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")
    
  | PELt (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lt(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PELte (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Lte(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

    | PEEq (e1, e2) -> 
      let (tt1, ee1) = transform_expr e1 env' ic in 
      let (tt2, ee2) = transform_expr e2 env' ic in 
      (match (attributes tt1).cmp, (attributes tt2).cmp with 
      | true, true -> TBool, Eq(ee1, ee2)
      | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")

  | PENeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    (match (attributes tt1).cmp, (attributes tt2).cmp with 
    | true, true -> TBool, Neq(ee1, ee2)
    | _, _ -> failwith @@ "Types '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' are not comparable")
      

  | PERef (i) -> 
    (match List.assoc_opt i ic with 
    | None -> Env.get_ref i env', LocalRef (i)
    | Some (t) -> t, LocalRef (i)
    )

  | PEApply (PERef("assert"), c) ->
    if List.length c <> 1 then failwith @@ "Assert need only one argument";
    let tt, ee = transform_expr (List.hd c) env' ic in
    if tt <> TBool then failwith @@ "Assert need a bool expression, got: " ^ show_ttype tt;
    TUnit, Assert(ee)

  | PEApply (e, el) -> 
    let (tt,ee) = transform_expr e env' ic in 
    (match tt with 
    | TLambda (TTuple(argl), rettype) when (List.length argl) = (List.length el) -> 
      let ap = List.map (fun (arg, ex) -> 
        let (ptt, pee) = transform_expr ex env' ic in 
        if ptt <> arg then 
          failwith @@ "Invalid argument on apply: got '" ^ show_ttype ptt ^ "' expected '" ^ show_ttype arg ^ "'" 
        else pee
      ) @@ List.combine argl el 
      in rettype, Apply(ee, Tuple(ap))

    | TLambda (TTuple(argl), _) when (List.length argl) <> (List.length el) -> 
      failwith @@ "Invalid argument number for lambda apply"

    | TLambda (t, rettype) when (List.length el) = 1 -> 
      let (ptt, pee) = transform_expr (List.hd el) env' ic in 
      if ptt <> t then 
        failwith @@ "Invalid argument for apply, got '" ^ show_ttype ptt ^ "' expected '" ^ show_ttype t ^ "'"
      else
        rettype, Apply(ee, pee)

    | TLambda (_, _) when (List.length el) > 1 -> 
      failwith "This lambda expect only one argument"
      
    | _ -> failwith "Applying on not a labmda")
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' ic in 
    let (te1, ee1) = transform_expr e1 env' ic in 
    let (te2, ee2) = transform_expr e2 env' ic in 
    (match tc, te1, te2 with 
    | TBool, t, t' when t <> t' -> failwith @@ "If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'"
    | TBool, t, t' when t = t' -> t, IfThenElse (ec, ee1, ee2)
    | _, _, _ -> failwith @@ "If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'")

  | PEMatchWith (e, bl) -> 
    let (te, ee) = transform_expr e env' ic in 
    let bl' = List.map (fun (cv, cex)  -> 
      let (tt, ee) = transform_expr cv env' ic in
      let (tcex, ecex) = transform_expr cex env' ic in
      if (tt <> te) then
        failwith @@ "Match case has an invalid value type, got: '" ^ show_ttype tt ^ "' expect '" ^ show_ttype te ^ "'"
      else 
        (ee, tcex, ecex) 
    ) bl in
    (* assert that every branch as the same type *)
    let rett: ttype = List.fold_left (fun acc (_, tcex, _) -> 
      if acc <> tcex then 
        failwith @@ "Match branches should have same type, got: '" ^ show_ttype tcex ^ "' expect '" ^ show_ttype acc ^ "'"
      else 
        tcex
    ) (let (_,b,_) = List.hd bl' in b) bl' 
    in rett, MatchWith (ee, List.map (fun (a,_,c) -> (a,c)) bl')


  | PELetIn(i, Some(t), e, e1) -> 
    let t' = transform_type t env' in
    let (tt, ee) = transform_expr e env' ic in 
    if tt <> t' then failwith @@ "LetIn type mismatch; got: '" ^ show_ttype tt ^ "' expect '" ^ show_ttype t' ^ "'";
    let (tt1, ee1) = transform_expr e1 env' @@ (i,t')::ic in 
    tt, LetIn (i, t', ee, ee1)

  | PESeq(PELet(i, Some(t), e), en) -> 
    let t' = transform_type t env' in
    let (tt, ee) = transform_expr e env' ic in 
    if tt <> t' then failwith @@ "Let type mismatch; got: '" ^ show_ttype tt ^ "' expect '" ^ show_ttype t' ^ "'";
    let (tnt, ene) = transform_expr en env' @@ (i, t')::ic in
    tnt, Seq(Let(i, t', ee), ene)

  | PELetIn(i, None, e, e1) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let (tt1, ee1) = transform_expr e1 env' @@ (i,tt)::ic in 
    tt, LetIn (i, tt, ee, ee1)

  | PESeq(PELet(i, None, e), en) -> 
    let (tt, ee) = transform_expr e env' ic in 
    let (tnt, ene) = transform_expr en env' @@ (i, tt)::ic in
    tnt, Seq(Let(i, tt, ee), ene)

  | PESeq (e1, e2) -> 
    let (tt1, ee1) = transform_expr e1 env' ic in 
    let (tt2, ee2) = transform_expr e2 env' ic in 
    if tt1 <> TUnit then failwith @@ "Cannot ignore non unit expression in sequence";
    (tt2, Seq(ee1, ee2))

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


(* | PELetIn of iden * ptype * pexpr * pexpr
| PELet of iden * ptype * pexpr 
| PELetTuple of (iden * ptype) list * pexpr 
| PESAssign of iden * pexpr
| PESRecAssign of iden * iden * pexpr 
| PECallBultin of iden * pexpr list *)
(* | PECall of left_op * iden * pexpr list  *)

(* 
| Parse_tree.PSCallBuiltin (i, el)::pl' ->
  let el' = List.map (fun x -> transform_expr x env') el in 
  
  (* check if we are calling a standard function *)
  let cmd = (match i with 
    | "assert" -> (match el' with 
      | [(TBool, e1)] -> Some (Assert(e1))
      | _ -> failwith "Invalid arguments for assert"
    )
    | "failif" -> (match el' with 
      | [(TBool, e1)] -> Some (FailIf(e1))
      | [(TBool, e1); (TString, e2)] -> Some (FailIfMessage(e1, e2))
      | _ -> failwith "Invalid arguments for failif"
    )
    | _ -> None
  ) in if cmd <> None then (Option.get cmd)::transform_statements pl' env' else 
    failwith @@ "Unknown builting function " ^ i     *)
