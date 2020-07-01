open Printf
open Helpers.Errors

type options = {
  contract: string option;
  target: string option;
  print_pt: bool;
  print_ast: bool;
  print_ligo: bool;
  verbose: bool;
  no_remove_unused: bool;
}

let default_options = {
  contract = None;
  target = Some ("tz");
  print_pt = true;
  print_ast = true;
  print_ligo = true;
  verbose = true;
  no_remove_unused = false;
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
  pt
  |> app opt.verbose @@ print_str "===> Injecting imports";
  |> Passes.Parse_tree_postprocess.inject_import

  (* print pt *)
  |> app opt.print_pt print_pt
  
  (* translate view to entry *)
  |> app opt.verbose @@ print_str "===> Transforming views to entries"
  |> Passes.Parse_tree_postprocess.contract_view_to_entry

  (* print pt *)
  |> app opt.print_pt print_pt

  (* transform pt to ast *)
  |> app opt.verbose @@ print_str "===> Translating Parse_tree to Ast"
  |> Passes.Parse_tree_to_ast.translate 

  (* print ast *)
  |> app opt.print_ast print_ast 

    
let write_file (filename: string) data = 
  let oc = open_out filename in 
  fprintf oc "%s" data;
  close_out oc 

(* text_file => ast => target *)
let compile (filename: string) opt =
  build_ast filename opt
    (* remove unused *)
    |> app opt.verbose @@ print_str "===> Dropping unused code" 
    |> ap (not opt.no_remove_unused) @@ Passes.Ast_remove_unused.remove_unused opt.contract
    |> app opt.print_ast print_ast 

    (* output to a final language *)
    |> (fun ast -> match opt.target, opt.contract with 
      | None, _ -> ""
      | Some ("tz"), Some(ctr)
      | Some ("ligo"), Some(ctr) -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Passes.Ast_to_ligo.generate_ligo ast ctr
      | Some ("tz"), None
      | Some ("ligo"), None when (List.length ast.contracts) = 1 -> 
        if opt.verbose then printf "===> Generating ligo code\n\n%!";        
        Passes.Ast_to_ligo.generate_ligo ast (fst @@ List.hd ast.contracts)
      | Some (_), None -> raise @@ CompilerError ("No contract specified for compilation")
    )
    |> (fun comp -> match opt.target with 
      | Some("tz") -> 
        if opt.print_ligo then comp |> print_endline;
        if opt.verbose then printf "===> Compilingo ligo to michelson\n\n%!";        
        write_file "/tmp/temp.mligo" comp;
        Sys.command "ligo compile-contract /tmp/temp.mligo main" |> ignore;
        ""
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
