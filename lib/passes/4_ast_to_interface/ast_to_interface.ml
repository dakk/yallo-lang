open Ast
open Ast_ttype
open Ast_env
open Helpers.Errors
open Helpers.Gen_utils

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)

let generate_interface (ast: Ast.t) (contract: string) = 
  let interface_of_contract ce = List.map (fun (a, b, c) -> (a, b)) ce.entries in
  let extract ci = Level ([
    Str ("interface " ^ contract ^ " {");
    Level (List.map (fun (i, tl) -> 
      Str ("entry " ^ i ^ "(" ^ merge_list tl ", " (fun (ti, tt) -> ti ^ ": " ^ show_ttype tt) ^ ");")
    ) ci);
    Str ("}")
  ]) in
  (match List.assoc_opt contract ast.contracts, List.assoc_opt contract ast.ifaces with 
  | None, None -> raise @@ CompilerError ("Unknown contract or interface '" ^ contract ^ "'")
  | Some(c), _ -> interface_of_contract c |> extract |> code_to_string
  | None, Some(ci) -> extract ci |> code_to_string)
