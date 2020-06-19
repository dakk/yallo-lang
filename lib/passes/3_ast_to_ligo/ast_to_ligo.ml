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

let rec to_ligo_expr (ast: t) ((te,e): texpr) = match e with 
| StorageEntry (i) -> "((Tezos.self \"%" ^ i ^ "\"): " ^ show_ttype te ^ ")"
| Entrypoint((te2, ContractInstance((tt,e))), i) -> 
  "match ((Tezos.get_entrypoint_opt \"%" ^ i ^ "\" (" ^ to_ligo_expr ast (tt,e) ^ ")): (" 
  ^ show_ttype te ^ ") option) with | None -> (failwith \"Invalid entrypoint\": " ^ show_ttype te ^ ") | Some (ep) -> ep"
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
| Unit -> "()"
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
(*
| ChainId of int
| Enum of ttype * string*)
| Typed (e, t) -> "(" ^ to_ligo_expr ast e ^ ": " ^ show_ttype t ^ ")"
| List (el) -> "[" ^ merge_list el "; " (fun e -> to_ligo_expr ast e) ^ "]"
| EnumValue (i) -> i

(* 
| Set of expr list 
| Map of (expr * expr) list
| BigMap of (expr * expr) list
*)
| Tuple (el) -> 
  "(" ^ merge_list el ", " (fun v -> to_ligo_expr ast v) ^ ")"
| Lambda (il, e) -> 
  "(fun (" ^ merge_list il ", " (fun (i,t) -> i ^ ": " ^ show_ttype t) ^ ") -> " ^ to_ligo_expr ast e ^ ")"

(* 
| Record of (iden * expr) list
| RecordAccess of expr * iden

(* map *)
| MapMem of expr * expr
| MapSize of expr
| MapMapWith of expr * expr
| MapFold of expr * expr * expr *)
| MapEmpty -> "Map.empty"
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
| BigMapGet (mape, vkey, vdef) ->
  "(match Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> " ^ to_ligo_expr ast vdef ^ " | Some (v) -> v)"
| BigMapGetOpt (mape, vkey) ->
  "Big_map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape
| BigMapUpdate (mape, vkey, vval) -> 
  let_surround ("Big_map.update (" ^ to_ligo_expr ast vkey ^ ") (Some (" ^ to_ligo_expr ast vval ^ ")) " ^ to_ligo_expr ast mape)
| BigMapRemove (mape, vkey) -> 
  let_surround ("Big_map.update (" ^ to_ligo_expr ast vkey ^ ") (None) " ^ to_ligo_expr ast mape)

(*
| BigMapMem of expr * expr

(* set *)
| SetEmpty
| SetMem of expr * expr
| SetSize of expr
| SetUpdate of expr * expr * expr 

(* list *)
| ListEmpty
| ListSize of expr
| ListPrepend of expr * expr
| ListMapWith of expr * expr
| ListHead of expr
| ListTail of expr
| ListFold of expr * expr * expr

(* string *)
| StringConcat of expr * expr 
| StringSlice of expr * expr * expr
| StringSize of expr
*)
| StringConcat (s1, s2) -> to_ligo_expr ast s1 ^ " ^ " ^ to_ligo_expr ast s2

(*

(* tuple *)
| TupleFst of expr
| TupleSnd of expr

(* aritmetic *) 
*)
| Add(a,b) -> "(" ^ to_ligo_expr ast a ^ ") + (" ^ to_ligo_expr ast b ^ ")"
| Sub(a,b) -> "(" ^ to_ligo_expr ast a ^ ") - (" ^ to_ligo_expr ast b ^ ")"
| Mul(a,b) -> "(" ^ to_ligo_expr ast a ^ ") * (" ^ to_ligo_expr ast b ^ ")"
| Div(a,b) -> "(" ^ to_ligo_expr ast a ^ ") / (" ^ to_ligo_expr ast b ^ ")"
(*
| Mod of expr * expr
| Abs of expr
| Ediv of expr * expr
| Neg of expr
| IsNat of expr
*)

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
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ") else (" ^ to_ligo_expr ast tee' ^ ")"
  | (e', te')::elle::el' -> 
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ") else " ^ rr @@ elle::el' 
  | (e', te')::[] -> 
    "if tmwttemp = (" ^ to_ligo_expr ast e' ^ ") then (" ^ to_ligo_expr ast te' ^ ") " 
  ) in "let tmwttemp = " ^ to_ligo_expr ast e ^ " in " ^ rr el

  
| FailIfMessage (e, m) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then failwith (" ^ to_ligo_expr ast m ^ ") else ()")
| FailIf (e) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then failwith \"Assertion\" else ()")
| Fail (e) -> let_surround ("failwith (" ^ to_ligo_expr ast e ^ ")")
| Assert (e) -> let_surround ("if (" ^ to_ligo_expr ast e ^ ") then () else failwith \"Assertion\"")
     
| Let (id, tt, e) -> "let " ^ id ^ ": " ^ show_ttype tt ^ " = " ^ to_ligo_expr ast e ^ " in"
| LetIn (id, tt, e, e2) -> 
  "let " ^ id ^ ": " ^ show_ttype tt ^ " = " ^ to_ligo_expr ast e ^ " in" ^ to_ligo_expr ast e2
| SAssign (i, e) -> "let s = { s with " ^ i ^ "=" ^ to_ligo_expr ast e ^ " } in"
(*
| LetTuple of (iden * ttype) list * expr 
| LetTupleIn of (iden * ttype) list * expr * expr
| SRecAssign of iden * iden * expr  *)

| Seq(a, (tl, List(e))) -> 
  "  " ^ to_ligo_expr ast a ^ "\n  ((" ^ to_ligo_expr ast (tl, List(e)) ^ ": operation list), (s: storage))"

| Seq(a, b) -> "  " ^ to_ligo_expr ast a ^ "\n" ^ to_ligo_expr ast b
(* | _ -> failwith @@ "Unable to generate ligo code for expression " ^ show_expr e *)
| _ -> "<<translation not handled: " ^ show_expr e ^ ">>"




let generate_ligo_code (ast: t) (contract: string) = 
  if List.assoc_opt contract ast.contracts = None then 
    raise @@ CompilerError ("Unknown contract '" ^ contract ^ "'");
  let (flds, ctor, entries) = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = (List.map (fun (i, (t,e)) -> 
    Str ("let " ^ i ^ " = " ^ to_ligo_expr ast (t,e) ^ "\n")
  ) ast.consts) in 

  (* generate the storage record *)
  let str = [
    if List.length flds = 0 then 
      Str("type storage = unit;\n\n")
    else 
      Str ("type storage = {\n" ^
      merge_list flds ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ show_ttype t) ^
      ";\n}");
    Empty; Empty
  ] in 

  (* generate the action variant *)
  let act = [ 
    Str("type action = "); 
    Level(List.map (fun (i, il, el) -> 
      Str("| " ^ String.capitalize_ascii i ^ 
        if List.length il > 0 then " of " ^ merge_list il " * " (fun (ii, it) -> show_ttype it)
        else " of unit")
      ) entries
    ); Empty; Empty
   ] in 

  (* write entries *)
  let entrs = List.map (fun (i, il, el) -> 
    Str("let " ^ i ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") il) ^
      "s: " ^ merge_list2 il " * " (fun (ii, it) -> show_ttype it) ^
      "storage) = \n" ^ to_ligo_expr ast el ^ "\n\n"
    )
  ) entries in

  (* write the main *)
  let main = [
    Str ("let main(a, s: action * storage): (operation list * storage) = ");
    Level([
      Str ("match a with");
      Level (List.map (fun (i, il, el) -> 
          Str ("| " ^ String.capitalize_ascii i ^ " (arg) -> " ^ 
          if List.length il > 0 then 
            "let (" ^ merge_list il ", " (fun (ii, it) -> ii)
            ^ ") = arg in " ^ i ^ "(" ^ merge_list2 il ", " (fun (ii, it) -> ii) ^ "s)"
          else 
            i ^ "(s)")
      ) entries)
    ])
  ] in
  Level (consts@str@act@entrs@main)


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract |> code_to_string