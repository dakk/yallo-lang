open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove contract *)

module SymbolSet = Set.Make(String)

let rec used_field_in_expr (t, e) = 
  traverse (t, e) (fun (t, e) -> 
    match e with 
    | SAssign (i, _) 
    | SRecAssign (i, _, _)
    | StorageRef (i) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let rec used_field_in_contract cname (fl, (ct1, ct2), elist) = 
  let l = (List.fold_left (fun acc (_, _, e) -> SymbolSet.union acc @@ used_field_in_expr e) SymbolSet.empty elist) in 
  let fl' = List.filter (fun (i, f) -> 
    if SymbolSet.mem i l then true else (
      Errors.emit_warning None "Unused field" @@ "The field '" ^ i ^ "' of contract '" ^ cname ^ "' is defined but never used; dropping from ast";
      false
    )
  ) fl in 
  cname, (fl', (ct1, ct2), elist)

let remove_unused (ctr: string option) ast = 
  {
    ast with contracts = List.map (fun (i, ce) -> used_field_in_contract i ce) ast.contracts
  }