open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Format
open Helpers.Gen_utils
open Big_int
open Pp_ctype

let pp_par fmt ((ti, tt): string * ttype) = 
  fprintf fmt "%s: %a" ti pp_ctype tt


let rec pp_cexpr fmt ((te,e): texpr) = 
  let let_surround fmt s = fprintf fmt "let %s = @[%a@] in " (temp_v ()) pp_cexpr s in
  let pp_infix2 fmt op a b = fprintf fmt "(%a) %s (%a)" pp_cexpr a op pp_cexpr b in
    
  match e with
  (* | StorageEntry (i) -> 
    fprintf fmt "((Tezos.self \"%%%s\"): %a)" i pp_ltype te
  
  | Entrypoint((te2, ContractInstance((tt,e))), (TString, String(i))) -> 
    fprintf fmt "match ((Tezos.get_entrypoint_opt \"%%%s\" (%a)): (%a) option) with @[| None -> (failwith \"Invalid entrypoint\": %a)@\n| Some (ep) -> ep@]"
      i 
      pp_cexpr (tt,e)
      pp_ltype te
      pp_ltype te *)
  
  (* 
  | ContractInstance of expr 
  
  | BuildContractCodeAndStorage of iden * expr list
  
  | Entrypoint of expr * iden
  
  | TezosAddressOfContract of expr
  *)
  
  (* | TezosContractOfAddress (ad) -> 
    fprintf fmt "match (Tezos.get_contract_opt %a : unit contract option) with @[| None -> (failwith \"invalid contract\": unit contract) @\n| Some(c) -> c@]"
      pp_cexpr ad
  
  | TezosNow -> 
    fprintf fmt "Tezos.now"
  
  | TezosAmount -> 
    fprintf fmt "Tezos.amount"
  
  | TezosBalance -> 
    fprintf fmt "Tezos.balance"
  
  | TezosChainId -> 
    fprintf fmt "Tezos.chain_id"
  
  | TezosSource -> 
    fprintf fmt "Tezos.source"
  
  | TezosSender -> 
    fprintf fmt "Tezos.sender"
  
  | TezosSelf -> 
    fprintf fmt "Tezos.self"
  
  | TezosSetDelegate (a) -> 
    fprintf fmt "Tezos.set_delegate (%a)" pp_cexpr a
  
  | TezosTransfer (ct, par, v) -> 
    fprintf fmt "Tezos.transaction @[@\n(%a)@\n(%a)@\n(%a)@]" 
      pp_cexpr par
      pp_cexpr v
      pp_cexpr ct
  
  | TezosSelfAddress -> 
    fprintf fmt "Tezos.self_address"
  
  | TezosCreateContract (cs, kho, mt) -> 
    fprintf fmt "let create_contract: (key_hash option * tez * innerStorage) -> (operation * address) =\n
    \t[%%Michelson ( {| \n
    {\n
      UNPPAIIR ; \n
      CREATE_CONTRACT \n
      #include \"oc.tz\" \n
      ; \n
      PAIR\n
    }\n
    \t|} : (key_hash option * tez * innerStorage) -> (operation * address))] in\n
    \tcreate_contract (%a) (%a)\n"
    pp_cexpr kho
    pp_cexpr mt
   *)
  (*
  | TezosImplicitAccount of expr
  *)
  
  (* | CryptoBlake2B (a) -> 
    fprintf fmt "Crypto.blake2b (%a)" pp_cexpr a
  
  | CryptoCheckSignature (a,b,c) -> 
    fprintf fmt "Crypto.check_signature (%a) (%a) (%a)" 
    pp_cexpr a
    pp_cexpr b
    pp_cexpr c
  
  | CryptoHashKey (a) -> 
    fprintf fmt "Crypto.hask_key (%a)" pp_cexpr a
  
  | CryptoSha256 (a) -> 
    fprintf fmt "Crypto.sha256 (%a)" pp_cexpr a
  
  | CryptoSha512 (a) -> 
    fprintf fmt "Crypto.sha512 (%a)" pp_cexpr a
  
  
  | GlobalRef (id)
  | LocalRef (id) -> 
    fprintf fmt "%s" id
  
  | StorageRef (id) -> 
    fprintf fmt "s.%s" id
  
  
  | None -> 
    fprintf fmt "None"  
  *)

  | Unit -> 
    fprintf fmt "unit"

  | Bool (i) -> 
    (match i with 
    | true -> fprintf fmt "True"
    | false -> fprintf fmt "False")

  | Nat (i) -> 
    fprintf fmt "%s" @@ string_of_big_int i

  | Int (i) -> 
    fprintf fmt "%s" @@ string_of_big_int i

  (*  
  | Mutez (i) -> 
    fprintf fmt "%smutez" @@ string_of_big_int i
  
  | Address (a) -> 
    fprintf fmt "(\"%s\": address)" a
  
  | String (s) -> 
    fprintf fmt "\"%s\"" s
  
  | Key (a) -> 
    fprintf fmt "(\"%s\": key)" a
  
  | KeyHash (a) -> 
    fprintf fmt "(\"%s\": key_hash)" a
  
  | Some(a) -> 
    fprintf fmt "Some (%a)" pp_cexpr a
  
  | Bytes (s) -> 
    fprintf fmt "(\"%s\": bytes)" (Bytes.to_string s)
  
  | Signature (s) -> 
    fprintf fmt "(\"%s\": signature)" s
  
  (* ChainId of int *)
  
  | EnumValue (i) -> 
    (match te with | TEnum(e) -> fprintf fmt "%dn" @@ enum_index e i 0)
  
  | Typed (e, t) -> 
    fprintf fmt "(%a: %a)" 
      pp_cexpr e
      pp_ltype t
    
  | List (el) -> 
    fprintf fmt "[ %a ]" (pp_list "; " pp_cexpr) el
  
  | Set (el) -> 
    fprintf fmt "set [ %a ]" (pp_list "; " pp_cexpr) el
  
  | Map (el) -> 
    let pp_el fmt (a, b) = fprintf fmt "(%a), (%a)" pp_cexpr a pp_cexpr b in 
    fprintf fmt "Map.literal [@[%a@]]" (pp_list "; " pp_el) el
    
  
  | BigMap (el) -> 
    let pp_el fmt (a, b) = fprintf fmt "(%a), (%a)" pp_cexpr a pp_cexpr b in 
    fprintf fmt "Big_map.literal [@[%a@]]" (pp_list "; " pp_el) el
  
  | Tuple (el) -> 
    fprintf fmt "( %a )" (pp_list ", " pp_cexpr) el
  
  | Lambda (il, e) -> 
    if List.length il = 0 then 
      fprintf fmt "( fun (override: unit) -> @[@\n%a@] )" pp_cexpr e 
    else 
      fprintf fmt "(fun (%a) -> @[%a@])"    
        pp_mpar il
        pp_cexpr e
  
  | Record (il) -> 
    let pp_rec_as fmt (i, e) = fprintf fmt "%s=%a" i pp_cexpr e in
    fprintf fmt "{ %a }"
      (pp_list "; " pp_rec_as) il
    
  | RecordAccess (e, i) -> 
    fprintf fmt "%a.%s" 
      pp_cexpr e 
      i *)
  
  (* option *)
  (* | OptionGetSome (oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> v | None -> failwith \"Expect some value\")" 
      pp_cexpr oe
  
  | OptionIsSome(oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> true | None -> false)" 
      pp_cexpr oe
  
  | OptionIsNone(oe) -> 
    fprintf fmt "(match (%a) with | Some(v) -> true | None -> true)" 
      pp_cexpr oe *)
  
  
  (* map *)
  (* | MapMem (mape, vkey) -> 
    fprintf fmt "(match Map.find_opt (%a) %a with | None -> false | Some (v) -> true)"
      pp_cexpr vkey
      pp_cexpr mape
  
  | MapFold (le, ll, initial) -> 
    fprintf fmt "Map.fold (%a) (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
      pp_cexpr initial
  
  | MapMapWith (le, ll) -> 
    fprintf fmt "Map.map (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
  
  | MapSize (mape) -> 
    fprintf fmt "Map.size (%a)" pp_cexpr mape
  
  | MapEmpty -> 
    fprintf fmt "Map.empty"
  
  | MapGetForce (mape, vkey) -> 
    fprintf fmt "(match Map.find_opt (%a) %a with | None -> ((failwith \"Key not present\"): %a) | Some (v) -> v)"
      pp_cexpr vkey
      pp_cexpr mape
      pp_ltype @@ (match fst mape with | TMap (a, b) -> b)
  
  | MapGet (mape, vkey, vdef) ->
    fprintf fmt "(match Map.find_opt (%a) %a with | None -> %a | Some (v) -> v)"
      pp_cexpr vkey
      pp_cexpr mape
      pp_cexpr vdef
  
  | MapGetOpt (mape, vkey) ->
    fprintf fmt "Map.find_opt (%a) %a" 
      pp_cexpr vkey
      pp_cexpr mape
  
  | MapUpdate (mape, vkey, vval) -> 
    fprintf fmt "Map.update (%a) (Some (%a)) %a" 
      pp_cexpr vkey
      pp_cexpr vval 
      pp_cexpr mape
  
  | MapRemove (mape, vkey) -> 
    fprintf fmt "Map.update (%a) (None) %a"
      pp_cexpr vkey
      pp_cexpr mape
    
   *)
  (* bigmap *)
  (* | BigMapEmpty -> 
    fprintf fmt "Big_map.empty"
  
  | BigMapMem (mape, vkey) -> 
    fprintf fmt "(match Big_map.find_opt (%a) %a  with | None -> false | Some (v) -> true)"
      pp_cexpr vkey
      pp_cexpr mape
  
  | BigMapGetForce (mape, vkey) -> 
    fprintf fmt "(match Big_map.find_opt (%a) %a with | None -> ((failwith \"Key not present\"): %a) | Some (v) -> v)"
      pp_cexpr vkey
      pp_cexpr mape
      pp_ltype @@ (match fst mape with | TMap (a, b) -> b)
  
  | BigMapGet (mape, vkey, vdef) ->
    fprintf fmt "(match Big_map.find_opt (%a) %a with | None -> %a | Some (v) -> v)"
      pp_cexpr vkey
      pp_cexpr mape
      pp_cexpr vdef
  
  | BigMapGetOpt (mape, vkey) ->
    fprintf fmt "Big_map.find_opt (%a) %a" 
      pp_cexpr vkey 
      pp_cexpr mape
  
  | BigMapUpdate (mape, vkey, vval) -> 
    fprintf fmt "Big_map.update (%a) (Some (%a)) %a"
      pp_cexpr vkey
      pp_cexpr vval
      pp_cexpr mape
  
  | BigMapRemove (mape, vkey) -> 
    fprintf fmt "Big_map.update (%a) (None) %a"
      pp_cexpr vkey
      pp_cexpr mape
  
  
  (* set *)
  | SetEmpty -> 
    fprintf fmt "Set.empty"
  
  | SetSize (le) ->
    fprintf fmt "Set.size (%a)" pp_cexpr le
  
  | SetMem (le, lv) ->
    fprintf fmt "Set.mem (%a) (%a)" 
      pp_cexpr lv
      pp_cexpr le
  
  | SetUpdate (se, sv, cc) -> 
    fprintf fmt "if (%a) then (Set.add (%a) (%a)) else (Set.remove (%a) (%a))"
      pp_cexpr cc
      pp_cexpr sv
      pp_cexpr se
      pp_cexpr sv
      pp_cexpr se
  
  (* list *)
  | ListEmpty -> 
    fprintf fmt "[]"
  
  | ListMapWith (le, ll) -> 
    fprintf fmt "List.map (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
  
  | ListPrepend (le, el) -> 
    fprintf fmt "(%a) :: (%a)"
      pp_cexpr el 
      pp_cexpr le
  
  | ListSize (le) ->
    fprintf fmt "List.size (%a)" pp_cexpr le
  
  | ListFold (le, ll, initial) -> 
    fprintf fmt "List.fold (%a) (%a) (%a)" 
      pp_cexpr ll
      pp_cexpr le
      pp_cexpr initial
  
  | ListFilter ((TList(lt), le), ll) ->
    fprintf fmt "List.fold (fun (acc, e: %a * %a) -> if (%a)(e) then e::acc else acc) (%a) ([]: %a)"
      pp_ltype (TList(lt))
      pp_ltype (lt)
      pp_cexpr ll
      pp_cexpr (TList(lt), le)
      pp_ltype (TList(lt)) *)
  
  (*
  | ListHead of expr
  | ListTail of expr
  
  (* string *)
  | StringSlice of expr * expr * expr
  *)
  (* | StringSize (s) -> 
    fprintf fmt "String.length (%a)" pp_cexpr s
  
  | StringConcat (a, b) -> 
    pp_infix2 fmt "^" a b *)
  
  (* bytes *)
  (* | BytesPack(a) -> 
    fprintf fmt "Bytes.pack (%a)" pp_cexpr a
  
  | BytesUnpack(a) -> 
    fprintf fmt "Bytes.unpack (%a)" pp_cexpr a
  
  | BytesSize (s) -> 
    fprintf fmt "Bytes.length (%a)" pp_cexpr s *)
  
  (* BytesSlice(a,b,c) *)
  
  (* | BytesConcat (a, b) -> 
    pp_infix2 fmt "^" a b
   *)
  
  (* tuple *)
  (*
  | TupleFst of expr
  | TupleSnd of expr
  *)
  
  (* aritmetic *) 
  (* | Add(a,b) -> 
    pp_infix2 fmt "+" a b
    
  | Sub(a,b) -> 
    pp_infix2 fmt "-" a b
    
  | Mul(a,b) -> 
    pp_infix2 fmt "*" a b
    
  | Div(a,b) -> 
    pp_infix2 fmt "/" a b
    
  | Abs(a) -> 
    fprintf fmt "abs(%a)" pp_cexpr a
  
  | ToInt(a) -> 
    fprintf fmt "int(%a)" pp_cexpr a
  
  | IsNat(a) -> 
    fprintf fmt "Michelson.is_nat(%a)" pp_cexpr a
  
  | Neg(a) -> 
    fprintf fmt "- (%a)" pp_cexpr a
  
  | Mod (a, b) -> 
    pp_infix2 fmt "mod" a b
   *)
  
  (* bool *)
  (* | Not(a) -> 
    fprintf fmt "! (%a)" pp_cexpr a
  
  | And(a,b) -> 
    pp_infix2 fmt "&&" a b
    
  | Or(a,b) -> 
    pp_infix2 fmt "||" a b
  
  | Lt (a, b) -> 
    pp_infix2 fmt "<" a b
    
  | Lte (a, b) -> 
    pp_infix2 fmt "<=" a b
  
  | Gt (a, b) -> 
    pp_infix2 fmt ">" a b
  
  | Gte (a, b) -> 
    pp_infix2 fmt ">=" a b
  
  | Eq (a, b) -> 
    pp_infix2 fmt "=" a b
  
  | Neq (a, b) -> 
    pp_infix2 fmt "<>" a b
  
  | IfThenElse (c, a, b) -> 
    fprintf fmt "(if %a then %a else %a)" 
      pp_cexpr c
      pp_cexpr a 
      pp_cexpr b
  
  | Apply((tce, Entrypoint((tci, ContractInstance(e)), i)), pp) ->
    fprintf fmt "(Tezos.transaction@[@\n(%a)@\n(0mutez)@\n(%a)@])"
      pp_cexpr pp
      pp_cexpr (tce, Entrypoint((tci, ContractInstance(e)), i))
  
  | Apply(lam, par) -> 
    fprintf fmt "%a (%a)" 
      pp_cexpr lam 
      pp_cexpr par
  
  | MatchWith (e, el) -> 
    let rec rr fmt el = (match el with 
    | [] -> fprintf fmt ""
    | (e', te')::((_, CaseDefault), tee')::el' -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse (%a: %a)"
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
        pp_cexpr tee'
        pp_ltype te
  
    | (e', te')::elle::el' -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse %a" 
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
        rr (elle::el')
  
    | (e', te')::[] -> 
      fprintf fmt "if %s = (%a) then @[(%a: %a)@] " 
        "tmatchwithtemp"
        pp_cexpr e'
        pp_cexpr te'
        pp_ltype te
    ) in 
    fprintf fmt "let %s = %a in @\n%a" 
      "tmatchwithtemp"
      pp_cexpr e
      rr el
  
    
  | FailIfMessage (e, m) -> 
    fprintf fmt "if (%a) then failwith (%a) else ()"
      pp_cexpr e
      pp_cexpr m
  
  | FailIf (e) -> 
    fprintf fmt "if (%a) then failwith \"Assertion\" else ()"
      pp_cexpr e
  
  | Fail (e) -> 
    fprintf fmt "failwith (%a)" pp_cexpr e
  
  | Assert (e) -> 
    fprintf fmt "if (%a) then () else (failwith \"Assertion\")" pp_cexpr e
  
  | Copy (e) -> 
    fprintf fmt "(%a)" pp_cexpr e
       
  | Let (id, tt, e) -> 
    fprintf fmt "let %s: %a = %a in " 
      id 
      pp_ltype tt
      pp_cexpr e
  
  | LetIn (id, tt, e, e2) -> 
    fprintf fmt "let %s: %a = @\n%a in @\n%a " 
      id 
      pp_ltype tt
      pp_cexpr e 
      pp_cexpr e2
  
  | LetTuple (il, e) -> 
    fprintf fmt "let (%a) = %a in "
      (pp_list ", " pp_str) (fst @@ List.split il)
      pp_cexpr e 
  
  | LetTupleIn (il, e, e2) -> 
    fprintf fmt "let (%a) = %a in @,%a"
      (pp_list ", " pp_str) (fst @@ List.split il)
      pp_cexpr e 
      pp_cexpr e2
  
  | SAssign (i, e) -> 
    fprintf fmt "let s = { s with %s=%a } in " 
      i 
      pp_cexpr e
  
  | SRecAssign (i, ii, expr) -> 
    fprintf fmt "let s = { s with %s= {s.%s with %s=%a} } in " 
      i i ii pp_cexpr expr
   *)

  | Seq(a, b) -> 
    fprintf fmt "@,%a@,%a"
  
    (match a with 
    | (TUnit, LetTuple(_, _)) 
    | (TUnit, LetTupleIn(_, _, _))
    | (TUnit, LetIn(_, _, _, _))
    | (TUnit, Let(_, _, _)) 
    | (TUnit, SAssign(_, _)) 
    | (TUnit, SRecAssign(_, _, _)) -> pp_cexpr
    | (TUnit, _) -> let_surround
    | _ -> pp_cexpr)
    a
    
    (match b with 
    | (tl, List(e)) -> (fun fmt un -> fprintf fmt "(%a: operation list)" pp_cexpr (tl, List(e)))
    | _ -> (fun fmt un -> fprintf fmt "%a" pp_cexpr b))
    ()

  | _ -> fprintf fmt "<<%s>>" (show_expr e)