open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Pp_cexpr
open Pp_ctype


let pp_pre fmt ce =
  let ll = [
    "Require Import List ZArith.";
    "Open Scope Z_scope.";
    "Open Scope list_scope."
  ] in
  (pp_list "@\n" pp_str) fmt ll


let pp_storage fmt fields = 
  if List.length fields = 0 then 
    fprintf fmt "Inductive storage : Type := unit.@\n@\n"
  else 
    fprintf fmt "Record storage : Set := Storage {@[@\n%a@]@\n}.@\n@\n"
    (pp_list ";@\n" pp_par) fields


let pp_consts fmt consts =
  let pp_const fmt (i, (t, e)) =
    fprintf fmt "Definition %s := @[%a@].@\n@\n" 
    i 
    pp_cexpr (t,e)
  in
  (pp_list "@\n" pp_const) fmt @@ List.rev consts
  

let pp_actions fmt entries = 
  let pp_act fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "%a : unit" pp_capit e.id
    else 
      fprintf fmt "%a : %a"
        pp_capit e.id
        (pp_list " -> " pp_ctype) (snd @@ List.split e.arg)
  in
  if List.length entries = 0 then fprintf fmt "@\n"
  else
    fprintf fmt "Inductive action : Type := @[@\n@ @ %a@].@\n@\n" 
    (pp_list "@\n| " pp_act) entries
 


let pp_main fmt entries = 
  let pp_actcall fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "%a (arg) => %s (s)" pp_capit e.id e.id
    else 
      fprintf fmt "%a (arg) => @[@\nlet (%a) = arg in %s (%a, s)@]"
      pp_capit e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
  in
  if List.length entries = 0 then (
    fprintf fmt "Definition main(a, s: unit * storage): (operation list * storage) := ([]: operation list), s end."
  ) else (
    fprintf fmt "Definition main(a, s: action * storage): (operation list * storage) := match a with @[@ @ %a@]@ @\nend."
    (pp_list "@\n| " pp_actcall) entries 
  )
    

let pp_entries fmt entries = 
  let pp_entry fmt e =
    fprintf fmt "Definition %s (%a%s: %a%s) := @[%a, (s:storage)@]@\n@\n"
      e.id 
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      (if List.length e.arg = 0 then "s" else ", s")
      (pp_list " * " pp_ctype) (snd @@ List.split e.arg)
      (if List.length e.arg = 0 then " storage" else " * storage")
      pp_cexpr e.expr    
  in
  fprintf fmt "%a@."
    (pp_list "@.@." pp_entry) entries


let generate_coq_code (ast: t) (contract: string) = 
  reset_temp ();
  let ce = List.assoc contract ast.contracts in

  (* import needed modules *)
  pp_pre sfmt ce;
  fprintf sfmt "@\n@\n";

  (* generate consts *)
  pp_consts sfmt ast.consts;

  (* generate storage *)
  pp_storage sfmt ce.fields;

  (* generate the action variant *)
  fprintf sfmt "@\n";
  pp_actions sfmt ce.entries;

  (* write entries *)
  pp_entries sfmt ce.entries;

  (* write the main *)
  pp_main sfmt ce.entries;

  sget ()


let generate_coq (ast: t) (contract: string) = 
  generate_coq_code ast contract