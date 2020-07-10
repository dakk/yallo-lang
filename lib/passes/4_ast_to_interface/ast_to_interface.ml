open Ast
open Format
open Ast_ttype
open Helpers.Errors
open Helpers.Gen_utils


let generate_interface (ast: Ast.t) (contract: string) = 
  let interface_of_contract ce = List.map (fun e -> (e.id, e.arg)) ce.entries in
  let pp_par fmt (ti, tt) = Format.fprintf fmt "%s: %s" ti @@ show_ttype tt in
  let pp_entry fmt (i, tl) =
    Format.fprintf fmt "  entry %s(%a);"
    i
    (pp_list3 ", " pp_par) 
    tl
  in

  let extract fmt ci = 
    Format.fprintf fmt "interface %s {@.@[%a@]@.}" 
      contract
      (pp_list3 "@." pp_entry) 
      ci
  in
  (match List.assoc_opt contract ast.contracts, List.assoc_opt contract ast.ifaces with 
  | None, None -> raise @@ CompilerError ("Unknown contract or interface '" ^ contract ^ "'")
  | Some(c), _ -> interface_of_contract c |> extract Format.str_formatter; flush_str_formatter ()
  | None, Some(ci) -> extract Format.str_formatter ci; flush_str_formatter ())
