type options = {
  dump_parse_tree: bool;
  dump_ast: bool;
}

let default_options = {
  dump_parse_tree = true;
  dump_ast = true;
}

let parse s = Parser.program Lexer.token (Lexing.from_string s)

let rec readfile ic = 
  try let line = input_line ic in (line ^ "\n")::(readfile ic) with _ -> close_in_noerr ic; []

(* parse a file from text to Parse_tree *)
let parse_file (filename: string): Parse_tree.t = 
  filename 
  |> open_in 
  |> readfile 
  |> List.fold_left (fun acc x -> acc ^ x) "" 
  |> parse

(* replace all imports in a Parse_tree with the content of the file *)
let rec inject_import (pt: Parse_tree.t): Parse_tree.t =
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DImport (path) -> (
      try ptl @ parse_file path |> inject_import
      with | e -> raise e)
    | _ -> ptl @ [dec]
  ) [] pt

(* dump the parse tree, debug only *)
let dump_pt (pt: Parse_tree.t) = pt |> Parse_tree.show |> print_endline; print_endline ""; pt

(* dump the ast tree, debug only *)
(* let dump_ast (ast: Ast.t) = ast |> Ast.show |> print_endline; print_endline ""; ast *)

(* conditionally returns f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

let compile (filename: string) (contract: string) opt =
  filename
  (* parse the starting file *)
  |> parse_file
  |> ap opt.dump_parse_tree dump_pt   
  (* parse and inject imports *)
  |> inject_import
  |> ap opt.dump_parse_tree dump_pt
  (* extract type of base, interface, function and contracts *)
  |> fun pt -> (Typecheck.extract_types pt, pt)
  |> fun (t, pt) -> t |> Typecheck.show |> print_endline; (t, pt)
  |> Typecheck.check_contracts_implement_extend
  (* |> Ast.from_parse_tree *)
  
  (* |> Typecheck.typecheck               *)
  (* check types of inner parsetree *)
  (* typechecker.extract_type_map *)
  (* typechecker. *)
  (* |> dump_ast *)
