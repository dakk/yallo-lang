open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Printf
open Helpers.Gen_utils

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l)
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)
let let_surround s = "let ovverraidable = " ^ s ^ " in"

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
| TLambda (p, r) -> "(" ^ to_ligo_type p ^ " -> " ^ to_ligo_type r ^ ")"
| TEnum (el) -> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " | ") ^ x) "" el
| TList (t) -> to_ligo_type t ^ " list"
| TSet (t) -> to_ligo_type t ^ " set"
| TMap (t, t') -> "(" ^ to_ligo_type t ^ ", " ^ to_ligo_type t' ^ ") map"
| TBigMap (t, t') -> "(" ^ to_ligo_type t ^ ", " ^ to_ligo_type t' ^ ") big_map"
| TOption (t) -> to_ligo_type t ^ " option"
| TRecord (l) -> "{ " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else "; ") ^ x ^ ": " ^ to_ligo_type xt) "" l ^ " }"
| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ to_ligo_type x) "" tl ^ ")"
| TContract (t) -> to_ligo_type t ^ " contract"
| _ -> raise @@ TypeError (None, "Type '" ^ show_ttype a ^ "' is not translable to ligo")

let rec to_ligo_expr (ast: t) ((te,e): texpr) = match e with 
| StorageEntry (i) -> "((Tezos.self \"%" ^ i ^ "\"): " ^ to_ligo_type te ^ ")"
| Entrypoint((te2, ContractInstance((tt,e))), i) -> 
  "match ((Tezos.get_entrypoint_opt \"%" ^ i ^ "\" (" ^ to_ligo_expr ast (tt,e) ^ ")): (" 
  ^ to_ligo_type te ^ ") option) with | None -> (failwith \"Invalid entrypoint\": " ^ to_ligo_type te ^ ") | Some (ep) -> ep"
(* | ContractInstance of expr 
| BuildContractCodeAndStorage of iden * expr list
| Entrypoint of expr * iden

| TezosAddressOfContract of expr*)
| TezosContractOfAddress (ad) -> 
  "match (Tezos.get_contract_opt " ^ to_ligo_expr ast ad ^ " : unit contract option) with"
  ^ "| None -> (failwith \"invalid contract\": unit contract) | Some(c) -> c"
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

(*
| TezosCreateContract of expr * expr * expr
| TezosImplicitAccount of expr
*)

| CryptoBlake2B (a) -> "Crypto.blake2b (" ^ to_ligo_expr ast a ^ ")"
| CryptoCheckSignature (a,b,c) -> "Crypto.check_signature (" ^ to_ligo_expr ast a ^ ") (" ^ to_ligo_expr ast b ^ ") (" ^ to_ligo_expr ast c ^ ")"
| CryptoHashKey (a) -> "Crypto.hask_key (" ^ to_ligo_expr ast a ^ ")"
| CryptoSha256 (a) -> "Crypto.sha256 (" ^ to_ligo_expr ast a ^ ")"
| CryptoSha512 (a) -> "Crypto.sha512 (" ^ to_ligo_expr ast a ^ ")"


| LocalRef (id) -> id
| StorageRef (id) -> "s." ^ id
| GlobalRef (id) -> id

| None -> "None"
| Unit -> "unit"
| Bool (i) -> sprintf "%b" i
| Nat (i) -> sprintf "%dn" i
| Int (i) -> sprintf "%d" i
| Mutez (i) -> sprintf "%dmutez" i
| Address (a) -> sprintf "(\"%s\": address)" a
| String (s) -> sprintf "\"%s\"" s
| Key (a) -> sprintf "(\"%s\": key)" a
| KeyHash (a) -> sprintf "(\"%s\": key_hash)" a
| Some(a) -> "Some (" ^ to_ligo_expr ast a ^ ")"
| Bytes (s) -> sprintf "(\"%s\": bytes)" (Bytes.to_string s)
| Signature (s) -> sprintf "(\"%s\": signature)" s
(* ChainId of int *)
| Enum (_, i) -> i
| Typed (e, t) -> "(" ^ to_ligo_expr ast e ^ ": " ^ to_ligo_type t ^ ")"
| List (el) -> "[" ^ merge_list el "; " (fun e -> to_ligo_expr ast e) ^ "]"
| EnumValue (i) -> i
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
  
| RecordAccess (e, i) -> to_ligo_expr ast e ^ "." ^ i

(* option *)
| OptionGetSome (oe) -> "(match (" ^ to_ligo_expr ast oe ^ ") with | Some(v) -> v | None -> failwith \"Expect some value\")"
| OptionIsSome(oe) -> "(match (" ^ to_ligo_expr ast oe ^ ") with | Some(v) -> true | None -> false"
| OptionIsNone(oe) -> "(match (" ^ to_ligo_expr ast oe ^ ") with | Some(v) -> true | None -> true"


(* map *)
| MapMem (mape, vkey) -> 
  "(match Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> false | Some (v) -> true)"
| MapFold (le, ll, initial) -> 
  "Map.fold (" ^ to_ligo_expr ast ll ^ ") (" ^ to_ligo_expr ast le ^ ") (" ^ to_ligo_expr ast initial ^ ")"
| MapMapWith (le, ll) -> 
  "Map.map (" ^ to_ligo_expr ast ll ^ ") (" ^ to_ligo_expr ast le ^ ")"
| MapSize (mape) -> "Map.size (" ^ to_ligo_expr ast mape ^ ")"
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
  let_surround ("Map.update (" ^ to_ligo_expr ast vkey ^ ") (Some (" ^ to_ligo_expr ast vval ^ ")) " ^ to_ligo_expr ast mape ^ "")
| MapRemove (mape, vkey) -> 
  let_surround ("Map.update (" ^ to_ligo_expr ast vkey ^ ") (None) " ^ to_ligo_expr ast mape)
  

(* bigmap *)
| BigMapEmpty -> "Big_map.empty"
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
| SetEmpty -> "Set.empty"
| SetSize (le) ->
  "Set.size (" ^ to_ligo_expr ast le ^ ")"
| SetMem (le, lv) ->
  "Set.mem (" ^ to_ligo_expr ast lv ^ ") (" ^ to_ligo_expr ast le ^ ")"
| SetUpdate (se, sv, cc) -> 
  "if (" ^ to_ligo_expr ast cc ^ ") then (Set.add (" ^ to_ligo_expr ast sv ^ ") (" ^ to_ligo_expr ast se ^ ")) else (Set.remove (" ^ to_ligo_expr ast sv ^ ") (" ^ to_ligo_expr ast se ^ "))"

(* list *)
| ListEmpty -> "[]"
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
| StringSize (s) -> "String.length (" ^ to_ligo_expr ast s ^ ")"
| StringConcat (s1, s2) -> to_ligo_expr ast s1 ^ " ^ " ^ to_ligo_expr ast s2

(* bytes *)
| BytesPack(a) -> "Bytes.pack (" ^ to_ligo_expr ast a ^ ")"
| BytesUnpack(a) -> "Bytes.unpack (" ^ to_ligo_expr ast a ^ ")"
| BytesSize (s) -> "Bytes.length (" ^ to_ligo_expr ast s ^ ")"
(* BytesSlice(a,b,c) *)
| BytesConcat (s1, s2) -> to_ligo_expr ast s1 ^ " ^ " ^ to_ligo_expr ast s2


(* tuple *)
(*
| TupleFst of expr
| TupleSnd of expr
*)

(* aritmetic *) 
| Add(a,b) -> "(" ^ to_ligo_expr ast a ^ ") + (" ^ to_ligo_expr ast b ^ ")"
| Sub(a,b) -> "(" ^ to_ligo_expr ast a ^ ") - (" ^ to_ligo_expr ast b ^ ")"
| Mul(a,b) -> "(" ^ to_ligo_expr ast a ^ ") * (" ^ to_ligo_expr ast b ^ ")"
| Div(a,b) -> "(" ^ to_ligo_expr ast a ^ ") / (" ^ to_ligo_expr ast b ^ ")"
| Abs(a) -> "abs(" ^ to_ligo_expr ast a ^ ")"
| ToInt(a) -> "int(" ^ to_ligo_expr ast a ^ ")"
| IsNat(a) -> "Michelson.is_nat(" ^ to_ligo_expr ast a ^ ")"
| Neg(a) -> "- (" ^ to_ligo_expr ast a ^ ")"
| Mod (a, b) -> "(" ^ to_ligo_expr ast a ^ ") mod (" ^ to_ligo_expr ast b ^ ")"


(* bool *)
| Not(a) -> "! (" ^ to_ligo_expr ast a ^ ")"
| And(a,b) -> "(" ^ to_ligo_expr ast a ^ ") && (" ^ to_ligo_expr ast b ^ ")"
| Or(a,b) -> "(" ^ to_ligo_expr ast a ^ ") || (" ^ to_ligo_expr ast b ^ ")"

| Lt (a, b) -> "(" ^ to_ligo_expr ast a ^ ") < (" ^ to_ligo_expr ast b ^ ")"
| Lte (a, b) -> "(" ^ to_ligo_expr ast a ^ ") <= (" ^ to_ligo_expr ast b ^ ")"
| Gt (a, b) -> "(" ^ to_ligo_expr ast a ^ ") > (" ^ to_ligo_expr ast b ^ ")"
| Gte (a, b) -> "(" ^ to_ligo_expr ast a ^ ") >= (" ^ to_ligo_expr ast b ^ ")"
| Eq (a, b) -> "(" ^ to_ligo_expr ast a ^ ") = (" ^ to_ligo_expr ast b ^ ")"
| Neq (a, b) -> "(" ^ to_ligo_expr ast a ^ ") <> (" ^ to_ligo_expr ast b ^ ")"

| IfThenElse (c, a, b) -> "(if " ^ to_ligo_expr ast c ^ " then " ^ to_ligo_expr ast a ^ " else " ^ to_ligo_expr ast b ^ ")"

| Apply((tce, Entrypoint((tci, ContractInstance(e)), i)), pp) ->
  "(Tezos.transaction (" ^ to_ligo_expr ast pp ^ ") (0mutez) (" ^ to_ligo_expr ast (tce, Entrypoint((tci, ContractInstance(e)), i)) ^"))"
| Apply(lam, par) -> 
  to_ligo_expr ast lam ^ "(" ^ to_ligo_expr ast par ^ ")"

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

  
| FailIfMessage (e, m) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then failwith (" ^ to_ligo_expr ast m ^ ") else ()")
| FailIf (e) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then failwith \"Assertion\" else ()")
| Fail (e) -> ("failwith (" ^ to_ligo_expr ast e ^ ")")
| Assert (e) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then () else (failwith \"Assertion\")")
| Copy (e) -> "(" ^ to_ligo_expr ast e ^ ")"
     
| Let (id, tt, e) -> "let " ^ id ^ ": " ^ to_ligo_type tt ^ " = " ^ to_ligo_expr ast e ^ " in "
| LetIn (id, tt, e, e2) -> 
  "let " ^ id ^ ": " ^ to_ligo_type tt ^ " = " ^ to_ligo_expr ast e ^ " in " ^ to_ligo_expr ast e2
| LetTuple (il, e) -> "let (" ^ merge_list il ", " (fun (id, t) -> id) ^ ") = " ^ to_ligo_expr ast e ^ " in "
| LetTupleIn (il, e, e2) -> "let (" ^ merge_list il ", " (fun (id, t) -> id) ^ ") = " ^ to_ligo_expr ast e ^ " in " ^ to_ligo_expr ast e2

| SAssign (i, e) -> "let s = { s with " ^ i ^ "=" ^ to_ligo_expr ast e ^ " } in "
| SRecAssign (i, ii, expr) -> "let s = { s with " ^ i ^ "= { s." ^ i ^ " with "^ ii ^"=" ^ to_ligo_expr ast expr ^ "}} in "

| Seq(a, (tl, List(e))) -> 
  "  " ^ to_ligo_expr ast a ^ "\n  (" ^ to_ligo_expr ast (tl, List(e)) ^ ": operation list)"

| Seq(a, b) -> "  " ^ to_ligo_expr ast a ^ "\n" ^ to_ligo_expr ast b
(* | _ -> failwith @@ "Unable to generate ligo code for expression " ^ show_expr e *)
| _ -> "<<translation not handled: " ^ show_expr e ^ ">>"




let generate_ligo_code (ast: t) (contract: string) = 
  if List.assoc_opt contract ast.contracts = None then 
    raise @@ CompilerError ("Unknown contract '" ^ contract ^ "'");
  let ce = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = (List.map (fun (i, (t,e)) -> 
    Str ("let " ^ i ^ " = " ^ to_ligo_expr ast (t,e) ^ "\n")
  ) ast.consts) in 

  (* generate the storage record *)
  let str = [
    if List.length ce.fields = 0 then 
      Str("type storage = unit\n\n")
    else 
      Str ("type storage = {\n" ^
      merge_list ce.fields ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ to_ligo_type t) ^
      ";\n}");
    Empty; Empty
  ] in 

  (* generate the action variant *)
  let act = [ 
    Str("type action = "); 
    Level(List.map (fun e -> 
      Str("| " ^ String.capitalize_ascii e.id ^ 
        if List.length e.arg > 0 then " of " ^ merge_list e.arg " * " (fun (ii, it) -> to_ligo_type it)
        else " of unit")
      ) ce.entries
    ); Empty; Empty
   ] in 

  (* write entries *)
  let entrs = List.map (fun e -> 
    Str("let " ^ e.id ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") e.arg) ^
      "s: " ^ merge_list2 e.arg " * " (fun (ii, it) -> to_ligo_type it) ^
      "storage) = \n" ^ to_ligo_expr ast e.expr ^ ", (s: storage)\n\n"
    )
  ) ce.entries in

  (* write the main *)
  let main = [
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
  ] in
  Level (consts@str@act@entrs@main)


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract |> code_to_string