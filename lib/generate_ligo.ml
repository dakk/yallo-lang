open Ast
open Ast_ttype
open Ast_expr
open Errors
open Printf

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l)
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)

let rec to_ligo_expr (ast: t) (e: expr) = match e with 
(* | ContractInstance of expr 
| StorageEntry of iden
| BuildContractCodeAndStorage of iden * expr list
| Entrypoint of expr * iden

| TezosNow
| TezosAmount
| TezosBalance
| TezosChainId
| TezosSelf
| TezosSetDelegate of expr
| TezosSource
| TezosSender
| TezosAddressOfContract of expr
| TezosContractOfAddress of expr *)
| TezosTransfer (ct, par, v) -> 
  "Tezos.transfer (" ^ to_ligo_expr ast ct ^ ") (" ^ to_ligo_expr ast par ^ ") (" ^ to_ligo_expr ast v ^ ")"

(*
| TezosCreateContract of expr * expr * expr
| TezosImplicitAccount of expr

| CryptoBlake2B of expr
| CryptoCheckSignature of expr * expr * expr
| CryptoHashKey of expr 
| CryptoSha256 of expr
| CryptoSha512 of expr
*)

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
(*
| ChainId of int
| Bytes of bytes
| Signature of string
| Enum of ttype * string*)
| Typed (e, t) -> "(" ^ to_ligo_expr ast e ^ ": " ^ show_ttype t ^ ")"
| List (el) -> "[" ^ merge_list el "; " (fun e -> to_ligo_expr ast e) ^ "]"
| EnumValue (i) -> i

(* 
| Set of expr list 
| Map of (expr * expr) list
| BigMap of (expr * expr) list
| Tuple of expr list *)
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
| MapGet (mape, vkey) ->
  "(match Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape 
  ^ " with | None -> failwith \"Missing key\" | Some (v) -> v)"
| MapGetOpt (mape, vkey) ->
  "Map.find_opt (" ^ to_ligo_expr ast vkey ^ ") " ^ to_ligo_expr ast mape
| MapUpdate (mape, vkey, vval) -> 
  "Map.update (" ^ to_ligo_expr ast vkey ^ ") (Some (" ^ to_ligo_expr ast vval ^ ")) " ^ to_ligo_expr ast mape ^ ";"
| MapRemove (mape, vkey) -> 
  "Map.update (" ^ to_ligo_expr ast vkey ^ ") (None) " ^ to_ligo_expr ast mape ^ ";"
  

(* bigmap *)
(*
| BigMapEmpty
| BigMapGetOpt of expr * expr
| BigMapGet of expr * expr * expr
| BigMapMem of expr * expr
| BigMapUpdate of expr * expr * expr 
| BigMapRemove of expr * expr 

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

(* 
| MatchWith of expr * (expr * expr) list
| Apply of expr * expr

| Fail of expr
| FailIf of expr
| FailIfMessage of expr * expr *)
| Assert (e) -> "assert(" ^ to_ligo_expr ast e ^ ");"
     
| Let (id, tt, e) -> "let " ^ id ^ ": " ^ show_ttype tt ^ " = " ^ to_ligo_expr ast e ^ " in"
| SAssign (i, e) -> "let s = { s with " ^ i ^ "=" ^ to_ligo_expr ast e ^ " } in"
(*
| LetIn of iden * ttype * expr * expr
| LetTuple of (iden * ttype) list * expr 
| LetTupleIn of (iden * ttype) list * expr * expr
| SRecAssign of iden * iden * expr  *)

| Seq(a, b) -> "  " ^ to_ligo_expr ast a ^ "\n  " ^ to_ligo_expr ast b
(* | _ -> failwith @@ "Unable to generate ligo code for expression " ^ show_expr e *)
| _ -> "Unable to generate ligo code for expression " ^ show_expr e

let generate_ligo (ast: t) (contract: string) = 
  if List.assoc_opt contract ast.contracts = None then 
    raise @@ GenerateLigoError ("Unknown contract '" ^ contract ^ "'");
  let (flds, ctor, entries) = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = list_to_string (List.map (fun (i, (t,e)) -> 
    "let " ^ i ^ " = " ^ to_ligo_expr ast e ^ "\n"
  ) ast.consts) in 

  (* generate the storage record *)
  let str = "type storage = {\n" ^
    merge_list flds ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ show_ttype t) ^
    ";\n}\n\n" in 

  (* generate the action variant *)
  let act = "type action = " ^ 
    (List.map (fun (i, il, el) -> "\n  | " ^ String.capitalize_ascii i ^ 
      if List.length il > 0 then " of " ^ merge_list il " * " (fun (ii, it) -> show_ttype it)
      else " of unit"
    ) entries |> list_to_string) ^ "\n\n"
  in 

  (* write entries *)
  let entrs = 
    (List.map (fun (i, il, el) -> 
      "let " ^ i ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") il) ^
      "s: " ^ merge_list2 il " * " (fun (ii, it) -> show_ttype it) ^
      "storage) = \n" ^ to_ligo_expr ast el ^ "\n\n"
    ) entries |> list_to_string) ^ "\n"
  in

  (* write the main *)
  let main = "let main(a, s: action * storage): (operation list * storage) = \n" ^
    "  match a with" ^
    (List.map (fun (i, il, el) -> 
      "\n  | " ^ String.capitalize_ascii i ^ " (arg) -> " ^ 
      if List.length il > 0 then 
        "let (" ^
        merge_list il ", " (fun (ii, it) -> ii)
        ^ ") = arg in " ^ i ^ "(" ^
        merge_list2 il ", " (fun (ii, it) -> ii)
        ^ "s)"
      else 
        i ^ "(s)"
    ) entries |> list_to_string) ^ "\n"

  in
  consts ^ "\n\n" ^ str ^ act ^ entrs ^ main