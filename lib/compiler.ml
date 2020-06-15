open Lexing
open Lexer
open Printf
open Errors

type options = {
  contract: string option;
  out_lang: string option;
  print_pt: bool;
  print_ast: bool;
  verbose: bool;
}

let default_options = {
  contract = None;
  out_lang = None;
  print_pt = true;
  print_ast = true;
  verbose = true;
}

let str_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse filename s = 
  let lexbuf = Lexing.from_string s in 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.program Lexer.token lexbuf with 
  | SyntaxError msg ->
    raise @@ SyntaxError(sprintf "%s: %s\n" (str_position lexbuf) msg)
  | Parser.Error ->
    raise @@ SyntaxError(sprintf "%s: syntax error\n" (str_position lexbuf))

let rec readfile ic = 
  try let line = input_line ic in (line ^ "\n")::(readfile ic) with _ -> close_in_noerr ic; []

(* parse a file from text to Parse_tree *)
let parse_file (filename: string): Parse_tree.t = 
  filename 
  |> open_in 
  |> readfile 
  |> List.fold_left (fun acc x -> acc ^ x) "" 
  |> parse filename

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
let print_pt (pt: Parse_tree.t) = pt |> Parse_tree.show |> print_endline; print_endline ""

(* dump the ast, debug only *)
let print_ast (ast: Ast.t) = ast |> Ast_env.Env.show |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


let compile (filename: string) opt =
  filename
    |> parse_file                   (* parse the starting file *)
    |> inject_import                (* parse and inject imports *)
    |> app opt.print_pt print_pt    (* print pt *)
    |> Ast.of_parse_tree            (* transform pt to ast *)
    |> app opt.print_ast print_ast  (* print ast *)
    |> fun ast -> match opt.out_lang, opt.contract with 
      | None, _ -> ()
      | Some (_), None -> failwith "No contract specified"
      | Some ("ligo"), Some(ctr) -> 
        print_endline @@ Generate_ligo.generate_ligo ast ctr;
        ()
    |> (fun _ -> ())
