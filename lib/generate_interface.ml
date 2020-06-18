open Ast
open Ast_ttype
open Errors

let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)
let interface_of_contract (_, _, el) = List.map (fun (a, b, c) -> (a, b)) el

let generate_interface (ast: t) (contract: string) = 
  let extract ci = "interface " ^ contract ^ " {\n"
  ^ list_to_string (List.map (fun (i, tl) -> 
    "  entry " ^ i ^ "(" ^ merge_list tl ", " (fun (ti, tt) -> ti ^ ": " ^ show_ttype tt) ^ ");\n"  
  ) ci) 
  ^ "}\n" in
  (match List.assoc_opt contract ast.contracts, List.assoc_opt contract ast.ifaces with 
  | None, None -> raise @@ CompilerError ("Unknown contract or interface '" ^ contract ^ "'")
  | Some(c), _ -> interface_of_contract c |> extract
  | None, Some(ci) -> extract ci)
