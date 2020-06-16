open Ast
open Ast_ttype
open Ast_expr
open Errors
open Printf

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)
let interface_of_contract (_, _, el) = List.map (fun (a, b, c) -> (a, b)) el

let generate_interface (ast: t) (contract: string) = 
  let iface = (match List.assoc_opt contract ast.symbols with 
  | None -> raise @@ CompilerError ("Unknown contract or interface '" ^ contract ^ "'")
  | Some(Contract) -> interface_of_contract @@ List.assoc contract ast.contracts 
  | Some(Interface) -> List.assoc contract ast.ifaces) in 
  
  "interface " ^ contract ^ " {\n"
  ^ list_to_string (List.map (fun (i, tl) -> 
    "  entry " ^ i ^ "(" ^ merge_list tl ", " (fun (ti, tt) -> ti ^ ": " ^ show_ttype tt) ^ ");\n"  
  ) iface) 
  ^ "}\n"
