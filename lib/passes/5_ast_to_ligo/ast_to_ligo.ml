open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l)
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)
let let_surround s = sprintf "let ovverraidable = %s in " s
let rec enum_index e i ii = match e with 
| [] -> failwith "Enum value not found"
| x::xe when x = i -> ii
| x::xe -> enum_index xe i (ii+1)


let rec to_ligo_type (a: ttype) = match a with
| TUnit -> "unit"
| TAddress -> "address"
| TInt -> "int"
| TChainId -> "chain_id"
| TOperation -> "operation"
| TNat -> "nat"
| TMutez -> "tez"
| TTimestamp -> "timestamp"
| TBool -> "bool"
| TSignature -> "signature"
| TKeyHash -> "key_hash"
| TKey -> "key"
| TString -> "string"
| TBytes -> "bytes"
| TLambda (p, r) -> sprintf "(%s -> %s)" (to_ligo_type p) (to_ligo_type r)
| TEnum (el) -> "nat"
(* List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " | ") ^ x) "" el *)
| TList (t) -> to_ligo_type t ^ " list"
| TSet (t) -> to_ligo_type t ^ " set"
| TMap (t, t') -> sprintf "(%s, %s) map" (to_ligo_type t) (to_ligo_type t')
| TBigMap (t, t') -> sprintf "(%s, %s) big_map" (to_ligo_type t) (to_ligo_type t')
| TOption (t) -> sprintf "%s option" @@ to_ligo_type t
| TRecord (l) -> "{ " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else "; ") ^ x ^ ": " ^ to_ligo_type xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ to_ligo_type x) "" tl ^ ")"
| TContract (t) -> to_ligo_type t ^ " contract"
| _ -> raise @@ TypeError (None, "Type '" ^ show_ttype a ^ "' is not translable to ligo")



let rec to_ligo_expr (ast: t) ((te,e): texpr) = 
  let pp_infix2 op a b = sprintf "(%s) %s (%s)" (to_ligo_expr ast a) op (to_ligo_expr ast b) in
  
match e with
| StorageEntry (i) -> sprintf "((Tezos.self \"%%%s\"): %s)" i (to_ligo_type te)

| Entrypoint((te2, ContractInstance((tt,e))), (TString, String(i))) -> 
  sprintf "match ((Tezos.get_entrypoint_opt \"%%%s\" (%s)): (%s) option) with | None -> (failwith \"Invalid entrypoint\": %s) | Some (ep) -> ep"
  i (to_ligo_expr ast (tt,e)) (to_ligo_type te) (to_ligo_type te)

(* 
| ContractInstance of expr 

| BuildContractCodeAndStorage of iden * expr list

| Entrypoint of expr * iden

| TezosAddressOfContract of expr
*)

| TezosContractOfAddress (ad) -> 
  sprintf "match (Tezos.get_contract_opt %s : unit contract option) with | None -> (failwith \"invalid contract\": unit contract) | Some(c) -> c"
  (to_ligo_expr ast ad)

| TezosNow -> "Tezos.now"

| TezosAmount -> "Tezos.amount"

| TezosBalance -> "Tezos.balance"

| TezosChainId -> "Tezos.chain_id"

| TezosSource -> "Tezos.source"

| TezosSender -> "Tezos.sender"

| TezosSelf -> "Tezos.self"

| TezosSetDelegate (a) -> "Tezos.set_delegate (" ^ to_ligo_expr ast a ^ ")"

| TezosTransfer (ct, par, v) -> 
  "Tezos.transaction (" ^ to_ligo_expr ast par ^ ") (" ^ to_ligo_expr ast v ^ ") (" ^ to_ligo_expr ast ct ^ ")"

| TezosSelfAddress -> "Tezos.self_address"

| TezosCreateContract (cs, kho, mt) -> 
  "let create_contract: (key_hash option * tez * innerStorage) -> (operation * address) =\n"
  ^ "\t[%Michelson ( {| \n"
  ^ "{\n"
  ^ "  UNPPAIIR ; \n"
  ^ "  CREATE_CONTRACT \n"
  ^ "  #include \"oc.tz\" \n"
  ^ "  ; \n"
  ^ "  PAIR\n"
  ^ "}\n"
  ^ "\t|} : (key_hash option * tez * innerStorage) -> (operation * address))] in\n"
  ^ "\tcreate_contract (" ^ to_ligo_expr ast kho ^ ") (" ^ to_ligo_expr ast mt ^ ")\n"
(*
| TezosImplicitAccount of expr
*)

| CryptoBlake2B (a) -> 
  sprintf "Crypto.blake2b (%s)" @@ to_ligo_expr ast a

| CryptoCheckSignature (a,b,c) -> 
  sprintf "Crypto.check_signature (%s) (%s) (%s)" 
  (to_ligo_expr ast a) 
  (to_ligo_expr ast b)
  (to_ligo_expr ast c)

| CryptoHashKey (a) -> 
  sprintf "Crypto.hask_key (%s)" @@ to_ligo_expr ast a

| CryptoSha256 (a) -> 
  sprintf "Crypto.sha256 (%s)" @@ to_ligo_expr ast a

| CryptoSha512 (a) -> 
  sprintf "Crypto.sha512 (%s)" @@ to_ligo_expr ast a


| LocalRef (id) -> id
| StorageRef (id) -> sprintf "s.%s" id
| GlobalRef (id) -> id


| None -> 
  sprintf "None"

| Unit -> 
  sprintf "unit"

| Bool (i) -> 
  sprintf "%b" i

| Nat (i) -> 
  sprintf "%dn" i

| Int (i) -> 
  sprintf "%d" i

| Mutez (i) -> 
  sprintf "%dmutez" i

| Address (a) -> 
  sprintf "(\"%s\": address)" a

| String (s) -> 
  sprintf "\"%s\"" s

| Key (a) -> 
  sprintf "(\"%s\": key)" a

| KeyHash (a) -> 
  sprintf "(\"%s\": key_hash)" a

| Some(a) -> 
  sprintf "Some (%s)" @@ to_ligo_expr ast a

| Bytes (s) -> 
  sprintf "(\"%s\": bytes)" (Bytes.to_string s)

| Signature (s) -> 
  sprintf "(\"%s\": signature)" s

(* ChainId of int *)

| EnumValue (i) -> (match te with | TEnum(e) -> sprintf "%dn" @@ enum_index e i 0)

| Typed (e, t) -> 
  sprintf "(%s: %s)" 
  (to_ligo_expr ast e) 
  (to_ligo_type t)
  
| List (el) -> "[" ^ merge_list el "; " (fun e -> to_ligo_expr ast e) ^ "]"
| Set (el) -> "set [" ^ merge_list el "; " (fun e -> to_ligo_expr ast e) ^ "]"

| Map (el) -> 
  "Map.literal [" ^
  merge_list el "; " (fun (a, b) -> "(" ^ to_ligo_expr ast a ^ "), (" ^ to_ligo_expr ast b ^ ")")
  ^ "]"

| BigMap (el) -> 
  "Big_map.literal [" ^
  merge_list el "; " (fun (a, b) -> "(" ^ to_ligo_expr ast a ^ "), (" ^ to_ligo_expr ast b ^ ")")
  ^ "]"

| Tuple (el) -> 
  "(" ^ merge_list el ", " (fun v -> to_ligo_expr ast v) ^ ")"

| Lambda (il, e) -> 
  (
    if List.length il = 0 then 
      "( fun (override: unit) "
    else 
      "(fun (" ^ merge_list il ", " (fun (i,t) -> i) ^ (if List.length il > 0 then ": " else "") ^ merge_list il " * " (fun (i,t) -> to_ligo_type t)  ^ ") "
  )
  ^ "-> " ^ to_ligo_expr ast e ^ ")"
  (* "(fun (" ^ merge_list (List.split il) ", " (fun (i,t) -> i ^ ": " ^ to_ligo_type t) ^ ") -> " ^ to_ligo_expr ast e ^ ")" *)

| Record (il) -> 
  "{ " ^ merge_list il "; " (fun (i, e) -> i ^ "=" ^ to_ligo_expr ast e) ^ " }"
  
| RecordAccess (e, i) -> 
  sprintf "%s.%s" (to_ligo_expr ast e) i

(* option *)
| OptionGetSome (oe) -> 
  sprintf "(match (%s) with | Some(v) -> v | None -> failwith \"Expect some value\")" (to_ligo_expr ast oe)

| OptionIsSome(oe) -> 
  sprintf "(match (%s) with | Some(v) -> true | None -> false)" (to_ligo_expr ast oe)

| OptionIsNone(oe) -> 
  sprintf "(match (%s) with | Some(v) -> true | None -> true)" (to_ligo_expr ast oe)


(* map *)
| MapMem (mape, vkey) -> 
  sprintf "(match Map.find_opt (%s) %s with | None -> false | Some (v) -> true)"
  (to_ligo_expr ast vkey)
  (to_ligo_expr ast mape)

| MapFold (le, ll, initial) -> 
  sprintf "Map.fold (%s) (%s) (%s)" 
  (to_ligo_expr ast ll)
  (to_ligo_expr ast le)
  (to_ligo_expr ast initial)

| MapMapWith (le, ll) -> 
  sprintf "Map.map (%s) (%s)" 
  (to_ligo_expr ast ll) 
  (to_ligo_expr ast le)

| MapSize (mape) -> 
  sprintf "Map.size (%s)" @@ to_ligo_expr ast mape

| MapEmpty -> "Map.empty"

| MapGetForce (mape, vkey) -> 
  let mapvt = match fst mape with | TMap (a, b) -> b in
  "(match Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> ((failwith \"Key not present\"): " ^ show_ttype mapvt ^ ") | Some (v) -> v)"
| MapGet (mape, vkey, vdef) ->
  "(match Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> " ^ to_ligo_expr ast vdef ^ " | Some (v) -> v)"
| MapGetOpt (mape, vkey) ->
  "Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape
| MapUpdate (mape, vkey, vval) -> 
  ("Map.update (" ^ to_ligo_expr ast vkey ^ ") (Some (" ^ to_ligo_expr ast vval ^ ")) " ^ to_ligo_expr ast mape ^ "")
| MapRemove (mape, vkey) -> 
  ("Map.update (" ^ to_ligo_expr ast vkey ^ ") (None) " ^ to_ligo_expr ast mape)
  

(* bigmap *)
| BigMapEmpty -> 
  sprintf "Big_map.empty"

| BigMapMem (mape, vkey) -> 
  "(match Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> false | Some (v) -> true)"
| BigMapGetForce (mape, vkey) -> 
  let mapvt = match fst mape with | TMap (a, b) -> b in
  "(match Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> ((failwith \"Key not present\"): " ^ show_ttype mapvt ^ ") | Some (v) -> v)"
| BigMapGet (mape, vkey, vdef) ->
  "(match Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> " ^ to_ligo_expr ast vdef ^ " | Some (v) -> v)"
| BigMapGetOpt (mape, vkey) ->
  "Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape
| BigMapUpdate (mape, vkey, vval) -> 
  "Big_map.update (" ^ to_ligo_expr ast vkey ^ ") (Some (" ^ to_ligo_expr ast vval ^ ")) " ^ to_ligo_expr ast mape
| BigMapRemove (mape, vkey) -> 
  "Big_map.update (" ^ to_ligo_expr ast vkey ^ ") (None) " ^ to_ligo_expr ast mape

(* set *)
| SetEmpty -> 
  sprintf "Set.empty"

| SetSize (le) ->
  sprintf "Set.size (%s)" @@ to_ligo_expr ast le

| SetMem (le, lv) ->
  "Set.mem (" ^ to_ligo_expr ast lv ^ ") (" ^ to_ligo_expr ast le ^ ")"
| SetUpdate (se, sv, cc) -> 
  "if (" ^ to_ligo_expr ast cc ^ ") then (Set.add (" ^ to_ligo_expr ast sv ^ ") (" ^ to_ligo_expr ast se ^ ")) else (Set.remove (" ^ to_ligo_expr ast sv ^ ") (" ^ to_ligo_expr ast se ^ "))"

(* list *)
| ListEmpty -> 
  sprintf "[]"

| ListMapWith (le, ll) -> 
  "List.map (" ^ to_ligo_expr ast ll ^ ") (" ^ to_ligo_expr ast le ^ ")"
| ListPrepend (le, el) -> 
  "(" ^ to_ligo_expr ast el ^ ") :: (" ^ to_ligo_expr ast le ^ ")"
| ListSize (le) ->
  "List.size (" ^ to_ligo_expr ast le ^ ")"
| ListFold (le, ll, initial) -> 
  "List.fold (" ^ to_ligo_expr ast ll ^ ") (" ^ to_ligo_expr ast le ^ ") (" ^ to_ligo_expr ast initial ^ ")"
| ListFilter ((TList(lt), le), ll) ->
  "List.fold (fun (acc, e: " ^ to_ligo_type (TList(lt)) ^ " * " ^ to_ligo_type (lt) ^ " ) -> if (" ^ to_ligo_expr ast ll ^ ")(e) then e::acc else acc) (" ^ to_ligo_expr ast (TList(lt), le) ^ ") ([]: " ^ to_ligo_type (TList(lt)) ^ ")"
(*
| ListHead of expr
| ListTail of expr

(* string *)
| StringSlice of expr * expr * expr
*)
| StringSize (s) -> 
  sprintf "String.length (%s)" @@ to_ligo_expr ast s

| StringConcat (a, b) -> 
  pp_infix2 "^" a b

(* bytes *)
| BytesPack(a) -> 
  sprintf "Bytes.pack (%s)" @@ to_ligo_expr ast a

| BytesUnpack(a) -> 
  sprintf "Bytes.unpack (%s)" @@ to_ligo_expr ast a

| BytesSize (s) -> 
  sprintf "Bytes.length (%s)" @@ to_ligo_expr ast s

(* BytesSlice(a,b,c) *)

| BytesConcat (a, b) -> 
  pp_infix2 "^" a b


(* tuple *)
(*
| TupleFst of expr
| TupleSnd of expr
*)

(* aritmetic *) 
| Add(a,b) -> 
  pp_infix2 "+" a b
  
| Sub(a,b) -> 
  pp_infix2 "-" a b
  
| Mul(a,b) -> 
  pp_infix2 "*" a b
  
| Div(a,b) -> 
  pp_infix2 "/" a b
  
| Abs(a) -> 
  sprintf "abs(%s)" @@ to_ligo_expr ast a

| ToInt(a) -> 
  sprintf "int(%s)" @@ to_ligo_expr ast a

| IsNat(a) -> 
  sprintf "Michelson.is_nat(%s)" @@ to_ligo_expr ast a

| Neg(a) -> 
  sprintf "- (%s)" @@ to_ligo_expr ast a

| Mod (a, b) -> 
  pp_infix2 "mod" a b


(* bool *)
| Not(a) -> 
  sprintf "! (%s)" @@ to_ligo_expr ast a

| And(a,b) -> 
  pp_infix2 "&&" a b
  
| Or(a,b) -> 
  pp_infix2 "||" a b

| Lt (a, b) -> 
  pp_infix2 "<" a b
  
| Lte (a, b) -> 
  pp_infix2 "<=" a b

| Gt (a, b) -> 
  pp_infix2 ">" a b

| Gte (a, b) -> 
  pp_infix2 ">=" a b

| Eq (a, b) -> 
  pp_infix2 "=" a b

| Neq (a, b) -> 
  pp_infix2 "<>" a b

| IfThenElse (c, a, b) -> 
  sprintf "(if %s then %s else %s)" 
  (to_ligo_expr ast c) 
  (to_ligo_expr ast a) 
  (to_ligo_expr ast b)

| Apply((tce, Entrypoint((tci, ContractInstance(e)), i)), pp) ->
  "(Tezos.transaction (" ^ to_ligo_expr ast pp ^ ") (0mutez) (" ^ to_ligo_expr ast (tce, Entrypoint((tci, ContractInstance(e)), i)) ^"))"

| Apply(lam, par) -> 
  sprintf "%s (%s)" 
  (to_ligo_expr ast lam) 
  (to_ligo_expr ast par)

| MatchWith (e, el) -> 
  let rec rr el = (match el with 
  | [] -> ""
  | (e', te')::((_, CaseDefault), tee')::el' -> 
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ": " ^ to_ligo_type te ^ ") else (" ^ to_ligo_expr ast tee' ^ ": " ^ to_ligo_type te ^ ")"
  | (e', te')::elle::el' -> 
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ": " ^ to_ligo_type te ^ ") else " ^ rr @@ elle::el' 
  | (e', te')::[] -> 
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ": " ^ to_ligo_type te ^ ") " 
  ) in "let tmwttemp = " ^ to_ligo_expr ast e ^ " in " ^ rr el

  
| FailIfMessage (e, m) -> 
  sprintf "if (%s) then failwith (%s) else ()"
  (to_ligo_expr ast e)
  (to_ligo_expr ast m)

| FailIf (e) -> 
  sprintf "if (%s) then failwith \"Assertion\" else ()"
  (to_ligo_expr ast e)

| Fail (e) -> 
  sprintf "failwith (%s)" @@ to_ligo_expr ast e

| Assert (e) -> 
  sprintf "if (%s) then () else (failwith \"Assertion\")" @@ to_ligo_expr ast e

| Copy (e) -> 
  sprintf "(%s)" @@ to_ligo_expr ast e
     
| Let (id, tt, e) -> 
  sprintf "let %s: %s = %s in " 
  id 
  (to_ligo_type tt) 
  (to_ligo_expr ast e)

| LetIn (id, tt, e, e2) -> 
  sprintf "let %s: %s = %s in %s " 
  id 
  (to_ligo_type tt) 
  (to_ligo_expr ast e) 
  (to_ligo_expr ast e2)

| LetTuple (il, e) -> 
  "let (" ^ merge_list il ", " (fun (id, t) -> id) ^ ") = " ^ to_ligo_expr ast e ^ " in "

| LetTupleIn (il, e, e2) -> 
  "let (" ^ merge_list il ", " (fun (id, t) -> id) ^ ") = " ^ to_ligo_expr ast e ^ " in " ^ to_ligo_expr ast e2

| SAssign (i, e) -> 
  sprintf "let s = { s with %s=%s } in " 
  i (to_ligo_expr ast e)

| SRecAssign (i, ii, expr) -> 
  sprintf "let s = { s with %s= {s.%s with %s=%s} } in " 
  i i ii (to_ligo_expr ast expr)

| Seq(a, b) -> 
  (match a with 
  | (TUnit, LetTuple(_, _)) -> to_ligo_expr ast a
  | (TUnit, LetTupleIn(_, _, _)) -> to_ligo_expr ast a
  | (TUnit, LetIn(_, _, _, _)) -> to_ligo_expr ast a
  | (TUnit, Let(_, _, _)) -> to_ligo_expr ast a
  | (TUnit, SAssign(_, _)) -> to_ligo_expr ast a
  | (TUnit, SRecAssign(_, _, _)) -> to_ligo_expr ast a
  | (TUnit, _) -> let_surround (to_ligo_expr ast a)
  | _ -> to_ligo_expr ast a)
  ^ "\n" ^
  (match b with 
  | (tl, List(e)) -> sprintf "(%s: operation list)" @@ to_ligo_expr ast (tl, List(e))
  | _ -> to_ligo_expr ast b)


(* | _ -> failwith @@ "Unable to generate ligo code for expression " ^ show_expr e *)
(* | _ -> "<<translation not handled: " ^ show_expr e ^ ">>" *)




let generate_ligo_code (ast: t) (contract: string) = 
  let ce = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = (List.map (fun (i, (t,e)) -> 
    Str ("let " ^ i ^ " = " ^ to_ligo_expr ast (t,e) ^ "\n")
  ) ast.consts) in 

  (* generate the storage record *)
  let str = [
    if List.length ce.fields = 0 then 
      Str("type storage = unit")
    else 
      Str ("type storage = {\n" ^
      merge_list ce.fields ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ to_ligo_type t) ^
      ";\n}");
    Empty; Empty
  ] in 

  (* generate the action variant *)
  let act = 
    if List.length ce.entries = 0 then ([])
    else (
    [ 
      Str("type action = "); 
      Level(List.map (fun e -> 
        Str("| " ^ String.capitalize_ascii e.id ^ 
          if List.length e.arg > 0 then " of " ^ merge_list e.arg " * " (fun (ii, it) -> to_ligo_type it)
          else " of unit")
        ) ce.entries
      ); Empty; Empty
    ]
  ) in 

  (* write entries *)
  let entrs = 
    List.map (fun e -> 
    Str("let " ^ e.id ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") e.arg) ^
      "s: " ^ merge_list2 e.arg " * " (fun (ii, it) -> to_ligo_type it) ^
      "storage) = \n" ^ to_ligo_expr ast e.expr ^ ", (s: storage)\n\n"
    )
  ) ce.entries in

  (* write the main *)
  let main =
    if List.length ce.entries = 0 then (
      [
        Str ("let main(a, s: unit * storage): (operation list * storage) = ");
        Str ("([]: operation list), s")
      ]
    ) else ([
      Str ("let main(a, s: action * storage): (operation list * storage) = ");
      Level([
        Str ("match a with");
        Level (List.map (fun e -> 
            Str ("| " ^ String.capitalize_ascii e.id ^ " (arg) -> " ^ 
            if List.length e.arg > 0 then 
              "let (" ^ merge_list e.arg ", " (fun (ii, it) -> ii)
              ^ ") = arg in " ^ e.id ^ "(" ^ merge_list2 e.arg ", " (fun (ii, it) -> ii) ^ "s)"
            else 
              e.id ^ "(s)")
        ) ce.entries)
      ])
    ])
  in
  Level (consts@str@act@entrs@main)


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract |> code_to_string