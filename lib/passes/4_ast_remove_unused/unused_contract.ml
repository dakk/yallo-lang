open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove contract *)

module SymbolSet = Set.Make(String)

let rec used_contract_in_expr (t, e) = 
  traverse (t, e) (fun (t, e) -> 
    match e with | BuildContractCodeAndStorage (i, _) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let rec used_contract_in_contract (_, (ct1, ct2), elist) = 
  (List.fold_left (fun acc (_, _, e) -> SymbolSet.union acc @@ used_contract_in_expr e) SymbolSet.empty elist)

let remove_unused (ctr: string option) ast = 
  let ctr = match ctr with 
  | None -> fst @@ List.hd ast.contracts
  | Some(c) -> c
  in 
  let used = (List.fold_left (fun acc (i, ce) -> SymbolSet.union acc (used_contract_in_contract ce)) SymbolSet.empty ast.contracts) in
  {
    ast with contracts = List.filter (fun (i, _) -> 
      if i = ctr || SymbolSet.mem i used then true else (
        Errors.emit_warning None "Unused contract" @@ "The contract '" ^ i ^ "' is defined but never used, dropping from ast";
        false)
    ) ast.contracts
  }