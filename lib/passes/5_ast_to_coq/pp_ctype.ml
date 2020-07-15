open Ast
open Ast_ttype
open Helpers.Errors
open Format
open Helpers.Gen_utils

let rec pp_ctype fmt a = match a with
| TBool -> 
  fprintf fmt "Prop"

| TNat -> 
  fprintf fmt "nat"

| TInt -> 
  fprintf fmt "Z"

| TUnit -> 
  fprintf fmt "unit"

| TEnum (el) -> 
  fprintf fmt "nat"

| TOption (t) -> 
  fprintf fmt "%a option" pp_ctype t

(*  
| TAddress -> 
  fprintf fmt "address"

| TChainId -> 
  fprintf fmt "chain_id"

| TOperation -> 
  fprintf fmt "operation"

| TMutez -> 
  fprintf fmt "tez"

| TTimestamp -> 
  fprintf fmt "timestamp"  

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


| TRecord (l) -> 
  let pp_rec_field fmt (x, xt) = fprintf fmt "%s: %a" x pp_ltype xt in
  fprintf fmt "{ @[%a@] }" 
    (pp_list ";@." pp_rec_field) l

| TContract (t) -> 
  fprintf fmt "%a contract" pp_ltype t *)

| TTuple (tl) -> 
  fprintf fmt "(%a)" 
    (pp_list " * " pp_ctype) tl

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to coq" (show_ttype a))