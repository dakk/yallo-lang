open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Big_int

let temp_i = ref 0
let temp_v () = temp_i := !temp_i + 1; sprintf "a__%d" !temp_i
let temp_c () = sprintf "a__%d" !temp_i

let rec enum_index e i ii = match e with 
| [] -> failwith "Enum value not found"
| x::xe when x = i -> ii
| x::xe -> enum_index xe i (ii+1)


let rec pp_ltype fmt (a: ttype) = match a with
| TUnit -> 
  fprintf fmt "unit"

| TAddress -> 
  fprintf fmt "address"

| TInt -> 
  fprintf fmt "int"

| TChainId -> 
  fprintf fmt "chain_id"

| TOperation -> 
  fprintf fmt "operation"

| TNat -> 
  fprintf fmt "nat"

| TMutez -> 
  fprintf fmt "tez"

| TTimestamp -> 
  fprintf fmt "timestamp"

| TBool -> 
  fprintf fmt "bool"

| TSignature -> 
  fprintf fmt "signature"

| TKeyHash -> 
  fprintf fmt "key_hash"

| TKey -> 
  fprintf fmt "key"

| TString -> 
  fprintf fmt "string"

| TBytes -> 
  fprintf fmt "bytes"

| TLambda (p, r) -> 
  fprintf fmt "(%a -> %a)" 
    pp_ltype p
    pp_ltype r

| TEnum (el) -> 
  fprintf fmt "nat"

| TList (t) -> 
  fprintf fmt "%a list" pp_ltype t

| TSet (t) -> 
  fprintf fmt "%a set" pp_ltype t

| TMap (t, t') -> 
  fprintf fmt "(%a, %a) map" 
    pp_ltype t
    pp_ltype t'

| TBigMap (t, t') -> 
  fprintf fmt "(%a, %a) big_map" 
    pp_ltype t 
    pp_ltype t'

| TOption (t) -> 
  fprintf fmt "%a option" pp_ltype t

| TRecord (l) -> 
  let pp_rec_field fmt (x, xt) = fprintf fmt "%s: %a" x pp_ltype xt in
  fprintf fmt "{ @[%a@] }" 
    (pp_list ";@." pp_rec_field) l

| TTuple (tl) -> 
  (* "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ pp_ltype x) "" tl ^ ")" *)
  fprintf fmt "(%a)" 
    (pp_list " * " pp_ltype) tl

| TContract (t) -> 
  fprintf fmt "%a contract" pp_ltype t

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to ligo" (show_ttype a))


let pp_par fmt ((ti, tt): string * ttype) = 
  fprintf fmt "%s: %a" ti pp_ltype tt

let pp_mpar fmt il =
  fprintf fmt "%a: %a"
    (pp_list ", " pp_str) (fst @@ List.split il)
    (pp_list " * " pp_ltype) (snd @@ List.split il)

let rec pp_lexpr fmt ((te,e): texpr) = 
  let let_surround fmt s = fprintf fmt "let %s = @[%a@] in " (temp_v ()) pp_lexpr s in
  let pp_infix2 fmt op a b = fprintf fmt "(%a) %s (%a)" pp_lexpr a op pp_lexpr b in
  
match e with
| StorageEntry (i) -> 
  fprintf fmt "((Tezos.self \"%%%s\"): %a)" i pp_ltype te

| Entrypoint((te2, ContractInstance((tt,e))), (TString, String(i))) -> 
  fprintf fmt "match ((Tezos.get_entrypoint_opt \"%%%s\" (%a)): (%a) option) with @[| None -> (failwith \"Invalid entrypoint\": %a)@\n| Some (ep) -> ep@]"
    i 
    pp_lexpr (tt,e)
    pp_ltype te
    pp_ltype te

(* 
| ContractInstance of expr 

| BuildContractCodeAndStorage of iden * expr list

| Entrypoint of expr * iden

| TezosAddressOfContract of expr
*)

| TezosContractOfAddress (ad) -> 
  fprintf fmt "match (Tezos.get_contract_opt %a : unit contract option) with @[| None -> (failwith \"invalid contract\": unit contract) @\n| Some(c) -> c@]"
    pp_lexpr ad

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
  fprintf fmt "Tezos.set_delegate (%a)" pp_lexpr a

| TezosTransfer (ct, par, v) -> 
  fprintf fmt "Tezos.transaction @[@\n(%a)@\n(%a)@\n(%a)@]" 
    pp_lexpr par
    pp_lexpr v
    pp_lexpr ct

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
  pp_lexpr kho
  pp_lexpr mt

(*
| TezosImplicitAccount of expr
*)

| CryptoBlake2B (a) -> 
  fprintf fmt "Crypto.blake2b (%a)" pp_lexpr a

| CryptoCheckSignature (a,b,c) -> 
  fprintf fmt "Crypto.check_signature (%a) (%a) (%a)" 
  pp_lexpr a
  pp_lexpr b
  pp_lexpr c

| CryptoHashKey (a) -> 
  fprintf fmt "Crypto.hask_key (%a)" pp_lexpr a

| CryptoSha256 (a) -> 
  fprintf fmt "Crypto.sha256 (%a)" pp_lexpr a

| CryptoSha512 (a) -> 
  fprintf fmt "Crypto.sha512 (%a)" pp_lexpr a


| GlobalRef (id)
| LocalRef (id) -> 
  fprintf fmt "%s" id

| StorageRef (id) -> 
  fprintf fmt "s.%s" id


| None -> 
  fprintf fmt "None"

| Unit -> 
  fprintf fmt "unit"

| Bool (i) -> 
  fprintf fmt "%b" i

| Nat (i) -> 
  fprintf fmt "%sn" @@ string_of_big_int i

| Int (i) -> 
  fprintf fmt "%s" @@ string_of_big_int i

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
  fprintf fmt "Some (%a)" pp_lexpr a

| Bytes (s) -> 
  fprintf fmt "(\"%s\": bytes)" (Bytes.to_string s)

| Signature (s) -> 
  fprintf fmt "(\"%s\": signature)" s

(* ChainId of int *)

| EnumValue (i) -> 
  (match te with | TEnum(e) -> fprintf fmt "%dn" @@ enum_index e i 0)

| Typed (e, t) -> 
  fprintf fmt "(%a: %a)" 
    pp_lexpr e
    pp_ltype t
  
| List (el) -> 
  fprintf fmt "[ %a ]" (pp_list "; " pp_lexpr) el

| Set (el) -> 
  fprintf fmt "set [ %a ]" (pp_list "; " pp_lexpr) el

| Map (el) -> 
  let pp_el fmt (a, b) = fprintf fmt "(%a), (%a)" pp_lexpr a pp_lexpr b in 
  fprintf fmt "Map.literal [@[%a@]]" (pp_list "; " pp_el) el
  

| BigMap (el) -> 
  let pp_el fmt (a, b) = fprintf fmt "(%a), (%a)" pp_lexpr a pp_lexpr b in 
  fprintf fmt "Big_map.literal [@[%a@]]" (pp_list "; " pp_el) el

| Tuple (el) -> 
  fprintf fmt "( %a )" (pp_list ", " pp_lexpr) el

| Lambda (il, e) -> 
  if List.length il = 0 then 
    fprintf fmt "( fun (override: unit) -> @[@\n%a@] )" pp_lexpr e 
  else 
    fprintf fmt "(fun (%a) -> @[%a@])"    
      pp_mpar il
      pp_lexpr e

| Record (il) -> 
  let pp_rec_as fmt (i, e) = fprintf fmt "%s=%a" i pp_lexpr e in
  fprintf fmt "{ %a }"
    (pp_list "; " pp_rec_as) il
  
| RecordAccess (e, i) -> 
  fprintf fmt "%a.%s" 
    pp_lexpr e 
    i

(* option *)
| OptionGetSome (oe) -> 
  fprintf fmt "(match (%a) with | Some(v) -> v | None -> failwith \"Expect some value\")" 
    pp_lexpr oe

| OptionIsSome(oe) -> 
  fprintf fmt "(match (%a) with | Some(v) -> true | None -> false)" 
    pp_lexpr oe

| OptionIsNone(oe) -> 
  fprintf fmt "(match (%a) with | Some(v) -> true | None -> true)" 
    pp_lexpr oe


(* map *)
| MapMem (mape, vkey) -> 
  fprintf fmt "(match Map.find_opt (%a) %a with | None -> false | Some (v) -> true)"
    pp_lexpr vkey
    pp_lexpr mape

| MapFold (le, ll, initial) -> 
  fprintf fmt "Map.fold (%a) (%a) (%a)" 
    pp_lexpr ll
    pp_lexpr le
    pp_lexpr initial

| MapMapWith (le, ll) -> 
  fprintf fmt "Map.map (%a) (%a)" 
    pp_lexpr ll
    pp_lexpr le

| MapSize (mape) -> 
  fprintf fmt "Map.size (%a)" pp_lexpr mape

| MapEmpty -> 
  fprintf fmt "Map.empty"

| MapGetForce (mape, vkey) -> 
  fprintf fmt "(match Map.find_opt (%a) %a with | None -> ((failwith \"Key not present\"): %a) | Some (v) -> v)"
    pp_lexpr vkey
    pp_lexpr mape
    pp_ltype @@ (match fst mape with | TMap (a, b) -> b)

| MapGet (mape, vkey, vdef) ->
  fprintf fmt "(match Map.find_opt (%a) %a with | None -> %a | Some (v) -> v)"
    pp_lexpr vkey
    pp_lexpr mape
    pp_lexpr vdef

| MapGetOpt (mape, vkey) ->
  fprintf fmt "Map.find_opt (%a) %a" 
    pp_lexpr vkey
    pp_lexpr mape

| MapUpdate (mape, vkey, vval) -> 
  fprintf fmt "Map.update (%a) (Some (%a)) %a" 
    pp_lexpr vkey
    pp_lexpr vval 
    pp_lexpr mape

| MapRemove (mape, vkey) -> 
  fprintf fmt "Map.update (%a) (None) %a"
    pp_lexpr vkey
    pp_lexpr mape
  

(* bigmap *)
| BigMapEmpty -> 
  fprintf fmt "Big_map.empty"

| BigMapMem (mape, vkey) -> 
  fprintf fmt "(match Big_map.find_opt (%a) %a  with | None -> false | Some (v) -> true)"
    pp_lexpr vkey
    pp_lexpr mape

| BigMapGetForce (mape, vkey) -> 
  fprintf fmt "(match Big_map.find_opt (%a) %a with | None -> ((failwith \"Key not present\"): %a) | Some (v) -> v)"
    pp_lexpr vkey
    pp_lexpr mape
    pp_ltype @@ (match fst mape with | TMap (a, b) -> b)

| BigMapGet (mape, vkey, vdef) ->
  fprintf fmt "(match Big_map.find_opt (%a) %a with | None -> %a | Some (v) -> v)"
    pp_lexpr vkey
    pp_lexpr mape
    pp_lexpr vdef

| BigMapGetOpt (mape, vkey) ->
  fprintf fmt "Big_map.find_opt (%a) %a" 
    pp_lexpr vkey 
    pp_lexpr mape

| BigMapUpdate (mape, vkey, vval) -> 
  fprintf fmt "Big_map.update (%a) (Some (%a)) %a"
    pp_lexpr vkey
    pp_lexpr vval
    pp_lexpr mape

| BigMapRemove (mape, vkey) -> 
  fprintf fmt "Big_map.update (%a) (None) %a"
    pp_lexpr vkey
    pp_lexpr mape


(* set *)
| SetEmpty -> 
  fprintf fmt "Set.empty"

| SetSize (le) ->
  fprintf fmt "Set.size (%a)" pp_lexpr le

| SetMem (le, lv) ->
  fprintf fmt "Set.mem (%a) (%a)" 
    pp_lexpr lv
    pp_lexpr le

| SetUpdate (se, sv, cc) -> 
  fprintf fmt "if (%a) then (Set.add (%a) (%a)) else (Set.remove (%a) (%a))"
    pp_lexpr cc
    pp_lexpr sv
    pp_lexpr se
    pp_lexpr sv
    pp_lexpr se

(* list *)
| ListEmpty -> 
  fprintf fmt "[]"

| ListMapWith (le, ll) -> 
  fprintf fmt "List.map (%a) (%a)" 
    pp_lexpr ll
    pp_lexpr le

| ListPrepend (le, el) -> 
  fprintf fmt "(%a) :: (%a)"
    pp_lexpr el 
    pp_lexpr le

| ListSize (le) ->
  fprintf fmt "List.size (%a)" pp_lexpr le

| ListFold (le, ll, initial) -> 
  fprintf fmt "List.fold (%a) (%a) (%a)" 
    pp_lexpr ll
    pp_lexpr le
    pp_lexpr initial

| ListFilter ((TList(lt), le), ll) ->
  fprintf fmt "List.fold (fun (acc, e: %a * %a) -> if (%a)(e) then e::acc else acc) (%a) ([]: %a)"
    pp_ltype (TList(lt))
    pp_ltype (lt)
    pp_lexpr ll
    pp_lexpr (TList(lt), le)
    pp_ltype (TList(lt))

(*
| ListHead of expr
| ListTail of expr

(* string *)
| StringSlice of expr * expr * expr
*)
| StringSize (s) -> 
  fprintf fmt "String.length (%a)" pp_lexpr s

| StringConcat (a, b) -> 
  pp_infix2 fmt "^" a b

(* bytes *)
| BytesPack(a) -> 
  fprintf fmt "Bytes.pack (%a)" pp_lexpr a

| BytesUnpack(a) -> 
  fprintf fmt "Bytes.unpack (%a)" pp_lexpr a

| BytesSize (s) -> 
  fprintf fmt "Bytes.length (%a)" pp_lexpr s

(* BytesSlice(a,b,c) *)

| BytesConcat (a, b) -> 
  pp_infix2 fmt "^" a b


(* tuple *)
(*
| TupleFst of expr
| TupleSnd of expr
*)

(* aritmetic *) 
| Add(a,b) -> 
  pp_infix2 fmt "+" a b
  
| Sub(a,b) -> 
  pp_infix2 fmt "-" a b
  
| Mul(a,b) -> 
  pp_infix2 fmt "*" a b
  
| Div(a,b) -> 
  pp_infix2 fmt "/" a b
  
| Abs(a) -> 
  fprintf fmt "abs(%a)" pp_lexpr a

| ToInt(a) -> 
  fprintf fmt "int(%a)" pp_lexpr a

| IsNat(a) -> 
  fprintf fmt "Michelson.is_nat(%a)" pp_lexpr a

| Neg(a) -> 
  fprintf fmt "- (%a)" pp_lexpr a

| Mod (a, b) -> 
  pp_infix2 fmt "mod" a b


(* bool *)
| Not(a) -> 
  fprintf fmt "! (%a)" pp_lexpr a

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
    pp_lexpr c
    pp_lexpr a 
    pp_lexpr b

| Apply((tce, Entrypoint((tci, ContractInstance(e)), i)), pp) ->
  fprintf fmt "(Tezos.transaction@[@\n(%a)@\n(0mutez)@\n(%a)@])"
    pp_lexpr pp
    pp_lexpr (tce, Entrypoint((tci, ContractInstance(e)), i))

| Apply(lam, par) -> 
  fprintf fmt "%a (%a)" 
    pp_lexpr lam 
    pp_lexpr par

| MatchWith (e, el) -> 
  let rec rr fmt el = (match el with 
  | [] -> fprintf fmt ""
  | (e', te')::((_, CaseDefault), tee')::el' -> 
    fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse (%a: %a)"
      "tmatchwithtemp"
      pp_lexpr e'
      pp_lexpr te'
      pp_ltype te
      pp_lexpr tee'
      pp_ltype te

  | (e', te')::elle::el' -> 
    fprintf fmt "if %s = (%a) then @[(%a: %a)@]@\nelse %a" 
      "tmatchwithtemp"
      pp_lexpr e'
      pp_lexpr te'
      pp_ltype te
      rr (elle::el')

  | (e', te')::[] -> 
    fprintf fmt "if %s = (%a) then @[(%a: %a)@] " 
      "tmatchwithtemp"
      pp_lexpr e'
      pp_lexpr te'
      pp_ltype te
  ) in 
  fprintf fmt "let %s = %a in @\n%a" 
    "tmatchwithtemp"
    pp_lexpr e
    rr el

  
| FailIfMessage (e, m) -> 
  fprintf fmt "if (%a) then failwith (%a) else ()"
    pp_lexpr e
    pp_lexpr m

| FailIf (e) -> 
  fprintf fmt "if (%a) then failwith \"Assertion\" else ()"
    pp_lexpr e

| Fail (e) -> 
  fprintf fmt "failwith (%a)" pp_lexpr e

| Assert (e) -> 
  fprintf fmt "if (%a) then () else (failwith \"Assertion\")" pp_lexpr e

| Copy (e) -> 
  fprintf fmt "(%a)" pp_lexpr e
     
| Let (id, tt, e) -> 
  fprintf fmt "let %s: %a = %a in " 
    id 
    pp_ltype tt
    pp_lexpr e

| LetIn (id, tt, e, e2) -> 
  fprintf fmt "let %s: %a = @\n%a in @\n%a " 
    id 
    pp_ltype tt
    pp_lexpr e 
    pp_lexpr e2

| LetTuple (il, e) -> 
  fprintf fmt "let (%a) = %a in "
    (pp_list ", " pp_str) (fst @@ List.split il)
    pp_lexpr e 

| LetTupleIn (il, e, e2) -> 
  fprintf fmt "let (%a) = %a in @,%a"
    (pp_list ", " pp_str) (fst @@ List.split il)
    pp_lexpr e 
    pp_lexpr e2

| SAssign (i, e) -> 
  fprintf fmt "let s = { s with %s=%a } in " 
    i 
    pp_lexpr e

| SRecAssign (i, ii, expr) -> 
  fprintf fmt "let s = { s with %s= {s.%s with %s=%a} } in " 
    i i ii pp_lexpr expr

| Seq(a, b) -> 
  fprintf fmt "@,%a@,%a"

  (match a with 
  | (TUnit, LetTuple(_, _)) 
  | (TUnit, LetTupleIn(_, _, _))
  | (TUnit, LetIn(_, _, _, _))
  | (TUnit, Let(_, _, _)) 
  | (TUnit, SAssign(_, _)) 
  | (TUnit, SRecAssign(_, _, _)) -> pp_lexpr
  | (TUnit, _) -> let_surround
  | _ -> pp_lexpr)
  a
  
  (match b with 
  | (tl, List(e)) -> (fun fmt un -> fprintf fmt "(%a: operation list)" pp_lexpr (tl, List(e)))
  | _ -> (fun fmt un -> fprintf fmt "%a" pp_lexpr b))
  ()



let pp_consts fmt consts = 
  let pp_const fmt (i, (t, e)) =
    fprintf fmt "let %s = @[%a@]@\n@\n" 
    i 
    pp_lexpr (t,e)
  in
  (pp_list "@\n" pp_const) fmt consts


let pp_storage fmt fields = 
  if List.length fields = 0 then 
    fprintf fmt "type storage = unit@\n@\n"
  else 
    fprintf fmt "type storage = {@[@\n%a@]@\n}@\n@\n"
    (pp_list ";@\n" pp_par) fields


let pp_actions fmt entries = 
  let pp_act fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "| %a of unit" pp_capit e.id
    else 
      fprintf fmt "| %a of %a"
        pp_capit e.id
        (pp_list " * " pp_ltype) (snd @@ List.split e.arg)
  in
  if List.length entries = 0 then fprintf fmt "@\n"
  else
    fprintf fmt "type action = @[@\n%a@]@\n@\n" 
    (pp_list "@\n" pp_act) entries
 


let pp_entries fmt entries = 
  let pp_entry fmt e =
    fprintf fmt "let %s (%a%s: %a%s) = @[%a, (s:storage)@]@\n@\n"
      e.id 
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      (if List.length e.arg = 0 then "s" else ", s")
      (pp_list " * " pp_ltype) (snd @@ List.split e.arg)
      (if List.length e.arg = 0 then " storage" else " * storage")
      pp_lexpr e.expr    
  in
  fprintf fmt "%a@."
    (pp_list "@.@." pp_entry) entries




let pp_main fmt entries = 
  let pp_actcall fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "| %a (arg) -> %s (s)" pp_capit e.id e.id
    else 
      fprintf fmt "| %a (arg) -> @[@\nlet (%a) = arg in %s (%a, s)"
      pp_capit e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
  in
  if List.length entries = 0 then (
    fprintf fmt "let main(a, s: unit * storage): (operation list * storage) = ([]: operation list), s"
  ) else (
    fprintf fmt "let main(a, s: action * storage): (operation list * storage) = match a with@[@\n%a@]"
    (pp_list "@\n" pp_actcall) entries 
  )



let generate_ligo_code (ast: t) (contract: string) = 
  let ce = List.assoc contract ast.contracts in

  (* dump const *)
  pp_consts sfmt ast.consts;

  (* generate the storage record *)
  pp_storage sfmt ce.fields;

  (* generate the action variant *)
  pp_actions sfmt ce.entries;

  (* write entries *)
  pp_entries sfmt ce.entries;

  (* write the main *)
  pp_main sfmt ce.entries;

  sget ()


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract