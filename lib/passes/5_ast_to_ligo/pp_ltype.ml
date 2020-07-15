open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Big_int

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
  fprintf fmt "(%a)" 
    (pp_list " * " pp_ltype) tl

| TContract (t) -> 
  fprintf fmt "%a contract" pp_ltype t

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to ligo" (show_ttype a))

