open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Printf
open Helpers.Gen_utils


let to_coq_type t = ""

let to_coq_expr ast (t, e) = 
  ""

let generate_coq_code (ast: t) (contract: string) = 
  let ce = List.assoc contract ast.contracts in

  let imps = [
    Str("Require ImportList  ZArith.");
    Str("Open ScopeZ_scope.");
    Str("Open Scopelist_scope.");
    Empty
  ] in

  (* dump const *)
  let consts = (List.map (fun (i, (t,e)) -> 
    Str ("Definition " ^ i ^ " = " ^ to_coq_expr ast (t,e) ^ ".\n")
  ) ast.consts) in 

  (* generate the storage record *)
  let str = [
    if List.length ce.fields = 0 then 
      Str("Inductive storage : Type := unit.")
    else 
      Str ("Inductive storage : Type := {\n" ^
      merge_list ce.fields ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ to_coq_type t) ^
      ";\n}");
    Empty; Empty
  ] in 

  (* generate the action variant *)
  let act = 
    if List.length ce.entries = 0 then ([])
    else (
    [ 
      Str("Inductive action : Type := "); 
      Level(List.map (fun e -> 
        Str("| " ^ String.capitalize_ascii e.id ^ 
          if List.length e.arg > 0 then " of " ^ merge_list e.arg " * " (fun (ii, it) -> to_coq_type it)
          else " of unit")
        ) ce.entries
      ); Empty; Empty
    ]
  ) in 

  (* write entries *)
  let entrs = 
    List.map (fun e -> 
    Str("Definition " ^ e.id ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") e.arg) ^
      "s: " ^ merge_list2 e.arg " * " (fun (ii, it) -> to_coq_type it) ^
      "storage) = \n" ^ to_coq_expr ast e.expr ^ ", (s: storage)\n\n"
    )
  ) ce.entries in

  (* write the main *)
  let main =
    if List.length ce.entries = 0 then (
      [
        Str ("Definition main (a, s: unit * storage): (operation list * storage) = ");
        Str ("([]: operation list), s")
      ]
    ) else ([
      Str ("Definition main(a, s: action * storage): (operation list * storage) = ");
      Level([
        Str ("match a with");
        Level (List.map (fun e -> 
            Str ("| " ^ String.capitalize_ascii e.id ^ " (arg) -> " ^ 
            if List.length e.arg > 0 then 
              "let (" ^ merge_list e.arg ", " (fun (ii, it) -> ii)
              ^ ") = arg in " ^ e.id ^ "(" ^ merge_list2 e.arg ", " (fun (ii, it) -> ii) ^ "s)"
            else 
              e.id ^ "(s)")
        ) ce.entries)
      ])
    ])
  in
  Level (imps@consts@str@act@entrs@main)


let generate_coq (ast: t) (contract: string) = 
  generate_coq_code ast contract |> code_to_string