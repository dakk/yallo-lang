open Ast
open Ast_ttype
open Ast_expr
open Errors

let rec list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l

let to_ligo_expr (ast: t) (e: expr) = "()"

let generate_ligo (ast: t) (contract: string) = 
  if List.assoc_opt contract ast.contracts = None then 
    raise @@ GenerateLigoError ("Unknown contract '" ^ contract ^ "'");
  let (flds, ctor, entries) = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = list_to_string (List.map (fun (i, (t,e)) -> 
    "let " ^ i ^ " = " ^ to_ligo_expr ast e ^ "\n"
  ) ast.consts) in 

  (* generate the storage record *)
  let str = "type storage = {\n" ^
    (List.map (fun (i, t) -> "  " ^ i ^ ": " ^ show_ttype t ^ ";\n") flds |> list_to_string) ^ 
    "}\n\n" in 

  (* generate the action variant *)
  let act = "type action = " ^ 
    (List.map (fun (i, il, el) -> "\n  | " ^ String.capitalize_ascii i ^ " of " ^ 
      list_to_string (List.mapi (fun i (ii,it) -> show_ttype it ^ (if i < (List.length il) - 1 then " * " else "")) il)
    ) entries |> list_to_string) ^ "\n\n"
  in 

  (* write entries *)
  let entrs = 
    (List.map (fun (i, il, el) -> 
      "let " ^ i ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") il) ^
      "s: " ^ list_to_string (List.mapi (fun i (ii,it) -> show_ttype it ^ " * ") il) ^
      "storage) = " ^ to_ligo_expr ast el ^ "\n\n"
    ) entries |> list_to_string) ^ "\n"
  in

  (* write the main *)
  let main = "let main(a, s: action * storage): (operation list * storage) = \n" ^
    "  match a with" ^
    (List.map (fun (i, il, el) -> 
      "\n  | " ^ String.capitalize_ascii i ^ " (arg) -> " ^ "let (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ (if i < (List.length il) - 1 then ", " else "")) il)
      ^ ") = arg in " ^ i ^ "(" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") il)
      ^ ", s"
    ) entries |> list_to_string) ^ "\n"

  in
  consts ^ "\n\n" ^ str ^ act ^ entrs ^ main