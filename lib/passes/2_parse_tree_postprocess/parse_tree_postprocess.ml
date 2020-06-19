(* replace all imports in a Parse_tree with the content of the file *)
let rec inject_import (pt: Parse_tree.t): Parse_tree.t =
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DImport (path) -> (
      try ptl @ Parsing.parse_file path |> inject_import
      with | e -> raise e)
    | _ -> ptl @ [dec]
  ) [] pt


(* handle pragma rules *)
let rec extract_pragma (pt: Parse_tree.t): (Parse_tree.t * string list) =
  (List.filter (fun i -> match i with | Parse_tree.DPragma(_) -> false | _ -> true) pt),
  (List.fold_left (fun opt dec -> 
    match dec with 
    | Parse_tree.DPragma (rule) -> rule::opt
    | _ -> opt
  ) [] pt)