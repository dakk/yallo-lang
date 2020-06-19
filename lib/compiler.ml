open Printf
open Helpers.Errors

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

(* dump the parse tree, debug only *)
let print_pt (pt: Intermediate.Parse_tree.t) = 
  pt 
  |> Intermediate.Parse_tree.show 
  |> print_endline; print_endline ""

(* dump the ast, debug only *)
let print_ast (ast: Intermediate.Ast.t) = 
  ast 
  |> Intermediate.Ast.show 
  |> print_endline; print_endline ""

let print_str s _ = s |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


(* text_file => ast *)
let build_ast (filename: string) opt =
  if opt.verbose then printf "===> Parsing %s\n\n%!" filename;

  (* parse the starting file *)
  let pt = filename |> Passes.Parsing.parse_file in

  (* extract and process pragma rules *)
  if opt.verbose then printf "===> Extracting pragma\n\n%!";
  let (pt, pragma_rules) = Passes.Parse_tree_postprocess.extract_pragma pt in 
  (* TODO: handle pragma rules *)

  (* parse and inject imports *)
  if opt.verbose then printf "===> Injecting imports\n\n%!";
  pt
  |> Passes.Parse_tree_postprocess.inject_import

  (* print pt *)
  |> app opt.print_pt print_pt

  (* transform pt to ast *)
  |> app opt.verbose @@ print_str "===> Translating Parse_tree to Ast"
  |> Passes.Parse_tree_to_ast.translate 

  (* print ast *)
  |> app opt.print_ast print_ast 

    
(* text_file => ast => out-lang *)
let compile (filename: string) opt =
  build_ast filename opt
    (* output to a final language *)
    |> (fun ast -> match opt.out_lang, opt.contract with 
      | None, _ -> ""
      | Some ("ligo"), Some(ctr) -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Passes.Ast_to_ligo.generate_ligo ast ctr
      | Some ("ligo"), None when (List.length ast.contracts) = 1 -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Passes.Ast_to_ligo.generate_ligo ast (fst @@ List.hd ast.contracts)
      | Some (_), None -> raise @@ CompilerError ("No contract specified for compilation")
    )
    |> print_endline


(* text_file => ast => interface *)
let extract_interface (filename: string) opt =
  build_ast filename opt
    |> (fun ast -> match opt.contract with 
      | None -> raise @@ CompilerError ("No contract specified for interface extraction")
      | Some(ctr) -> 
        if opt.verbose then printf "===> Extracting interface\n\n%!";        
        Passes.Ast_to_interface.generate_interface ast ctr
    )
    |> print_endline
