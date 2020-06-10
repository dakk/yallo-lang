type options = {
  contract: string option;
  print_pt: bool;
  print_ast: bool;
}

let default_options = {
  contract = None;
  print_pt = true;
  print_ast = true;
}

let parse s = Lexing.from_string s |> Parser.program Lexer.token

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
let print_pt (pt: Parse_tree.t) = pt |> Parse_tree.show |> print_endline; print_endline ""

(* [ap b f] conditionally apply f or iden if b or not b *)
let ap b f = if b then f else (fun x -> x)

(* [app b f] conditionally apply f or iden if b or not b, return the same value *)
let app b f = if b then (fun x -> let _: unit = f x in x) else (fun x -> x)


let compile (filename: string) opt =
  filename
    |> parse_file                   (* parse the starting file *)
    |> inject_import                (* parse and inject imports *)
    |> app opt.print_pt print_pt     (* print pt *)

    |> (fun x -> ())
