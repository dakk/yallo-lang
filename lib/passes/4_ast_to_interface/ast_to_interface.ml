open Ast
open Ast_ttype
open Format
open Helpers.Errors
open Helpers.Gen_utils


let generate_interface (ast: Ast.t) (contract: string) = 
  let interface_of_contract ce = 
    List.map (fun e -> (e.id, e.arg)) ce.entries in
  let pp_par fmt (ti, tt) = 
    fprintf fmt "%s: %s" ti @@ show_ttype tt in
  let pp_entry fmt (i, tl) =
    fprintf fmt "entry %s(%a);" i (pp_list ", " pp_par) tl in

  let extract fmt ci = 
    fprintf fmt "interface %s {@[@\n%a@]@.}" 
      contract (pp_list "@\n" pp_entry) ci
  in
  (match List.assoc_opt contract ast.contracts, List.assoc_opt contract ast.ifaces with 
  | None, None -> raise @@ CompilerError (sprintf "Unknown contract or interface '%s'" contract)
  | Some(c), _ -> 
    c 
    |> interface_of_contract
    |> extract sfmt
    |> sget
  | None, Some(ci) -> 
    ci 
    |> extract sfmt
    |> sget
  )
