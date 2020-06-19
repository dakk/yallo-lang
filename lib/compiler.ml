open Parsing
open Printf
open Helpers.Errors
open Parse_tree
open Ast
open Parse_tree_to_ast


type options = {
  contract: string option;
  out_lang: string option;
  print_pt: bool;
  print_ast: bool;
  verbose: bool;
}

let default_options = {
  contract = None;
  out_lang = Some ("ligo");
  print_pt = true;
  print_ast = true;
  verbose = true;
}




let rec readfile ic = 
  try let line = input_line ic in (line ^ "\n")::(readfile ic) with _ -> close_in_noerr ic; []

(* parse a file from text to Parse_tree *)
let parse_file (filename: string): Parse_tree.t = 
  filename 
  |> open_in 
  |> readfile 
  |> List.fold_left (fun acc x -> acc ^ x) "" 
  |> Parsing.parse filename

(* replace all imports in a Parse_tree with the content of the file *)
let rec inject_import (pt: Parse_tree.t): Parse_tree.t =
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DImport (path) -> (
      try ptl @ parse_file path |> inject_import
      with | e -> raise e)
    | _ -> ptl @ [dec]
  ) [] pt

let rec extract_pragma (pt: Parse_tree.t) opt: (Parse_tree.t * options) =
  (List.filter (fun i -> match i with | Parse_tree.DPragma(_) -> false | _ -> true) pt),
  (List.fold_left (fun opt dec -> 
    match dec with 
    | Parse_tree.DPragma (rule) -> opt
    | _ -> opt
  ) opt pt)


(* dump the parse tree, debug only *)
let print_pt (pt: Parse_tree.t) = pt |> Parse_tree.show |> print_endline; print_endline ""

(* dump the ast, debug only *)
let print_ast (ast: Ast.t) = ast |> Ast.show |> print_endline; print_endline ""

let print_str s t = s |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


let build_ast (filename: string) opt =
  if opt.verbose then printf "===> Parsing %s\n\n%!" filename;
  let pt = filename |> parse_file in       (* parse the starting file *)
  if opt.verbose then printf "===> Extracting pragma\n\n%!";
  let (pt, opt) = extract_pragma pt opt in (* extract and process pragma rules *)
  if opt.verbose then printf "===> Injecting imports\n\n%!";
  pt|> inject_import                (* parse and inject imports *)
    |> app opt.print_pt print_pt    (* print pt *)
    |> app opt.verbose @@ print_str "===> Translating Parse_tree to Ast"
    |> Parse_tree_to_ast.translate  (* transform pt to ast *)
    |> app opt.print_ast print_ast  (* print ast *)

    
    
let compile (filename: string) opt =
  build_ast filename opt
    (* output to a final language *)
    |> (fun ast -> match opt.out_lang, opt.contract with 
      | None, _ -> ""
      | Some ("ligo"), Some(ctr) -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Ast_to_ligo.generate_ligo ast ctr
      | Some ("ligo"), None when (List.length ast.contracts) = 1 -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Ast_to_ligo.generate_ligo ast (fst @@ List.hd ast.contracts)
      | Some (_), None -> raise @@ CompilerError ("No contract specified for compilation")
    )
    |> print_endline


let extract_interface (filename: string) opt =
  build_ast filename opt
    |> (fun ast -> match opt.contract with 
      | None -> raise @@ CompilerError ("No contract specified for interface extraction")
      | Some(ctr) -> 
        if opt.verbose then printf "===> Extracting interface\n\n%!";        
        Ast_to_interface.generate_interface ast ctr
    )
    |> print_endline
