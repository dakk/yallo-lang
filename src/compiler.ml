open Lexing
open Lexer
open Printf

type options = {
  contract: string option;
  print_pt: bool;
  print_ast: bool;
  verbose: bool;
}

let default_options = {
  contract = None;
  print_pt = true;
  print_ast = true;
  verbose = true;
}

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse filename s = 
  let lexbuf = Lexing.from_string s in 
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try Parser.program Lexer.token lexbuf with 
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

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

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


let compile (filename: string) opt =
  filename
    |> parse_file                   (* parse the starting file *)
    |> inject_import                (* parse and inject imports *)
    |> app opt.print_pt print_pt    (* print pt *)
    |> Typed.of_parse_tree

    |> (fun x -> ())
