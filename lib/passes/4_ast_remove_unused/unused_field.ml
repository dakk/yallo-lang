open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove unused fields *)

module SymbolSet = Set.Make(String)

let rec used_field_in_expr (t, e) = 
  traverse (t, e) (fun (t, e) -> 
    match e with 
    | SAssign (i, _) 
    | SRecAssign (i, _, _)
    | StorageRef (i) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let rec used_field_in_contract cname ce = 
  let l = (List.fold_left (fun acc e -> SymbolSet.union acc @@ used_field_in_expr e.expr) SymbolSet.empty ce.entries) in 
  let fl' = List.filter (fun (i, f) -> 
    if SymbolSet.mem i l then true else (
      Errors.emit_warning None "Unused field" @@ "The field '" ^ i ^ "' of contract '" ^ cname ^ "' is defined but never used, dropping from ast";
      false
    )
  ) ce.fields in 
  cname, { ce with fields=fl' }

let remove_unused (ctr: string option) (ast: Ast.t) = 
  {
    ast with contracts = List.map (fun (i, ce) -> used_field_in_contract i ce) ast.contracts
  }