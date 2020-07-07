open Ast
open Ast_expr
open Ast_expr_traversal
open Helpers

(* remove unused consts *)

module SymbolSet = Set.Make(String)

let used_globalref_in_expr (t, e) = 
  traverse (t, e) (fun (_, e) -> 
    match e with | GlobalRef (i) -> SymbolSet.singleton i
  ) SymbolSet.union SymbolSet.empty
  
let used_globalref_in_contract ce = 
  SymbolSet.union
    (List.fold_left (fun acc e -> SymbolSet.union acc @@ used_globalref_in_expr e.expr) SymbolSet.empty ce.entries)
    (List.fold_left (fun acc (_, e) -> SymbolSet.union acc @@ used_globalref_in_expr e) SymbolSet.empty @@ ce.constructor.exprs)

let remove_unused _ (ast: Ast.t) = 
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