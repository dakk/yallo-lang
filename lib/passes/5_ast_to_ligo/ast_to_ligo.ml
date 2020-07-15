open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils
open Big_int
open Pp_ltype
open Pp_lexpr


let pp_consts fmt consts = 
  let pp_const fmt (i, (t, e)) =
    fprintf fmt "let %s = @[%a@]@\n@\n" 
    i 
    pp_lexpr (t,e)
  in
  (pp_list "@\n" pp_const) fmt @@ List.rev consts


let pp_storage fmt fields = 
  if List.length fields = 0 then 
    fprintf fmt "type storage = unit@\n@\n"
  else 
    fprintf fmt "type storage = {@[@\n%a@]@\n}@\n@\n"
    (pp_list ";@\n" pp_par) fields


let pp_actions fmt entries = 
  let pp_act fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "| %a of unit" pp_capit e.id
    else 
      fprintf fmt "| %a of %a"
        pp_capit e.id
        (pp_list " * " pp_ltype) (snd @@ List.split e.arg)
  in
  if List.length entries = 0 then fprintf fmt "@\n"
  else
    fprintf fmt "type action = @[@\n%a@]@\n@\n" 
    (pp_list "@\n" pp_act) entries
 


let pp_entries fmt entries = 
  let pp_entry fmt e =
    fprintf fmt "let %s (%a%s: %a%s) = @[%a, (s:storage)@]@\n@\n"
      e.id 
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      (if List.length e.arg = 0 then "s" else ", s")
      (pp_list " * " pp_ltype) (snd @@ List.split e.arg)
      (if List.length e.arg = 0 then " storage" else " * storage")
      pp_lexpr e.expr    
  in
  fprintf fmt "%a@."
    (pp_list "@.@." pp_entry) entries




let pp_main fmt entries = 
  let pp_actcall fmt e =
    if List.length e.arg = 0 then 
      fprintf fmt "| %a (arg) -> %s (s)" pp_capit e.id e.id
    else 
      fprintf fmt "| %a (arg) -> @[@\nlet (%a) = arg in %s (%a, s)@]"
      pp_capit e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
      e.id
      (pp_list ", " pp_str) (fst @@ List.split e.arg)
  in
  if List.length entries = 0 then (
    fprintf fmt "let main(a, s: unit * storage): (operation list * storage) = ([]: operation list), s"
  ) else (
    fprintf fmt "let main(a, s: action * storage): (operation list * storage) = match a with@[@\n%a@]"
    (pp_list "@\n" pp_actcall) entries 
  )



let generate_ligo_code (ast: t) (contract: string) = 
  reset_temp ();
  let ce = List.assoc contract ast.contracts in

  (* dump const *)
  pp_consts sfmt ast.consts;

  (* generate the storage record *)
  pp_storage sfmt ce.fields;

  (* generate the action variant *)
  pp_actions sfmt ce.entries;

  (* write entries *)
  pp_entries sfmt ce.entries;

  (* write the main *)
  pp_main sfmt ce.entries;

  sget ()


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract