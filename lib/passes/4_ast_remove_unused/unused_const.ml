open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove unused const *)

module SymbolSet = Set.Make(String)

let rec used_globalref_in_expr (t, e) = 
  traverse (t, e) (fun (t, e) -> 
    match e with | GlobalRef (i) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let rec used_globalref_in_contract (_, (ct1, ct2), elist) = 
  SymbolSet.union
    (List.fold_left (fun acc (_, _, e) -> SymbolSet.union acc @@ used_globalref_in_expr e) SymbolSet.empty elist)
    (List.fold_left (fun acc (_, e) -> SymbolSet.union acc @@ used_globalref_in_expr e) SymbolSet.empty ct2)

let remove_unused ctr ast = 
  let used = SymbolSet.union 
    (List.fold_left (fun acc (i,ce) -> SymbolSet.union acc (used_globalref_in_expr ce)) SymbolSet.empty ast.consts)
    (List.fold_left (fun acc (i, ce) -> SymbolSet.union acc (used_globalref_in_contract ce)) SymbolSet.empty ast.contracts) 
  in
  {
    ast with consts = List.filter (fun (i, _) -> 
      if SymbolSet.mem i used then true else (
        Errors.emit_warning None "Unused constant" @@ "The constant '" ^ i ^ "' is not used, dropping from ast";
        false)
    ) ast.consts
  }