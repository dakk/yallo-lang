open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove unused contract *)

module SymbolSet = Set.Make(String)

let rec used_contract_in_expr (t, e) = 
  traverse (t, e) (fun (t, e) -> 
    match e with | BuildContractCodeAndStorage (i, _) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let rec used_contract_in_contract ce = 
  (List.fold_left (fun acc e -> SymbolSet.union acc @@ used_contract_in_expr e.expr) SymbolSet.empty ce.entries)

let remove_unused (ctr: string option) (ast: Ast.t) = 
  if List.length ast.contracts = 0 then ast else 
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