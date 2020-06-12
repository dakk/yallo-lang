open Ast_ttype
open Ast_env
open Ast_expr
open Translate_ptype

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

  (* PEApply(PETRef) tezos apis *)
  | PEApply (PETRef (i), el) ->
    let el' = List.map (fun a -> transform_expr a env') el in 
    (match i, el' with 
      | _, _ -> failwith @@ "Unknown Tezos function: " ^ i
    )

  (* PEApply(PECRef) crypto apis *)
  | PEApply (PECRef (i), el) -> 
    let el' = List.map (fun a -> transform_expr a env') el in 
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
    let (te, ee) = transform_expr e env' in
    let el' = List.map (fun a -> transform_expr a env') el in 
    (match te, i, el' with 
      (* List *)
      | TList (_), "size", [] -> TNat, ListSize (ee)
      | TList (l), "head", [] -> l, ListHead (ee)
      | TList (l), "tail", [] -> TList (l), ListTail (ee)
      | TList (l), "prepend", [(ll, e)] when ll = l -> TList (l), ListPrepend (ee, e)
      | TList (l), "mapWith", [(TLambda (ll, rt), lame)] when l = ll -> TList (rt), ListMapWith (ee, lame)

      (* Map *)
      | TMap (_, _), "size", [] -> TNat, MapSize (ee)
      | TMap (kt, kv), "get", [(kk, e)] when kk = kt -> kv, MapGet(ee, e)
      | TMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, MapMem(ee, e)
      | TMap (kt, kv), "mapWith", [(TLambda (TTuple([a;b]), rt), lame)] when (a=kt && b=kv) -> 
        TMap (kt, rt), MapMapWith (ee, lame)

      (* BigMap *)
      | TBigMap (kt, kv), "get", [(kk, e)] when kk = kt -> kv, BigMapGet(ee, e)
      | TBigMap (kt, _), "mem", [(kk, e)] when kk = kt -> TBool, BigMapMem(ee, e)

      (* Set *)
      | TSet (_), "size", [] -> TNat, SetSize (ee)
      | TSet (kt), "mem", [(ll, e)] when kt = ll -> TBool, SetMem (ee, e)

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
    let (te, ee) = transform_expr e env' in
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
    | TString, TKeyHash, String (a) -> TKeyHash, KeyHash (a)
    | TString, TKey, String (a) -> TKey, Key (a)
    | TString, TSignature, String (a) -> TSignature, Signature (a)
    | TString, TAddress, String (a) -> TAddress, Address (a)
    | TString, TBytes, String (a) -> TBytes, Bytes (Bytes.of_string a)
    | TBytes, TString, Bytes (a) -> TString, String (Bytes.to_string a)
    | TOption (TAny), TOption(t), None -> TOption(t), None
    | a, b, _ when a=b -> a, ee
    | a, b, c -> failwith @@ "Invalid cast from '" ^ show_ttype a ^ "' to '" ^ show_ttype b ^ "' for value: " ^ show_expr c)


  | PELambda (argl, e) -> 
    let rl = List.map (fun (i,t) -> i, transform_type t env') argl in
    let (tt, ee) = transform_expr e @@ Env.push_scope env' (Scope.of_params Lambda rl) in 
    let arg = (match List.length rl with 
      | 0 -> TUnit
      | 1 -> snd @@ List.hd rl
      | _ -> TTuple (snd @@ List.split rl)
    ) in
    TLambda (arg, tt), Lambda(rl, ee)

  | PERecord (l) -> 
    let l' = List.map (fun (i,e) -> i, transform_expr e env') l in 
    let (idtt, idee) = List.map (fun (i, (tt, ee)) -> (i, tt), (i, ee)) l' |> List.split in
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
      | _, _ -> failwith @@ "Add between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
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
      | _, _ -> failwith @@ "Mul between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
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
      | _, _ -> failwith @@ "Sub between '" ^ show_ttype tt1 ^ "' and '" ^ show_ttype tt2 ^ "' is not allowed"
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

    | TLambda (TTuple(argl), _) when (List.length argl) <> (List.length el) -> 
      failwith @@ "Invalid argument number for lambda apply"

    | TLambda (t, rettype) when (List.length el) = 1 -> 
      let (ptt, pee) = transform_expr (List.hd el) env' in 
      if ptt <> t then 
        failwith @@ "Invalid argument for apply, got '" ^ show_ttype ptt ^ "' expected '" ^ show_ttype t ^ "'"
      else
        rettype, Apply(ee, pee)

    | TLambda (_, _) when (List.length el) > 1 -> 
      failwith "This lambda expect only one argument"
      
    | _ -> failwith "Applying on not a labmda")
  

  | PEIfThenElse (c, e1, e2) -> 
    let (tc, ec) = transform_expr c env' in 
    let (te1, ee1) = transform_expr e1 env' in 
    let (te2, ee2) = transform_expr e2 env' in 
    (match tc, te1, te2 with 
    | TBool, t, t' when t <> t' -> failwith @@ "If branches should have same type, got: '" ^ show_ttype t ^ "' and '" ^ show_ttype t' ^ "'"
    | TBool, t, t' when t = t' -> t, IfThenElse (ec, ee1, ee2)
    | _, _, _ -> failwith @@ "If condition should be a boolean expression, got '" ^ show_ttype tc ^ "'")

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
    let rett: ttype = List.fold_left (fun acc (_, tcex, _) -> 
      if acc <> tcex then 
        failwith @@ "Match branches should have same type, got: '" ^ show_ttype tcex ^ "' expect '" ^ show_ttype acc ^ "'"
      else 
        tcex
    ) (let (_,b,_) = List.hd bl' in b) bl' 
    in rett, MatchWith (ee, List.map (fun (a,_,c) -> (a,c)) bl')

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
