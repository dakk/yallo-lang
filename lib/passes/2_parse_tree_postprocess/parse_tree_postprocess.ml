open Parse_tree

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


let rec contract_view_to_entry (pt: Parse_tree.t): Parse_tree.t =
  List.fold_left (fun ptl dec -> 
    match dec with 
    | Parse_tree.DInterface (di) -> (
      let signatures = List.map (fun (de: signature) -> 
        (match de.ret with 
        | None -> de 
        | Some(ret) -> { de with 
          ret=None;
          arg=de.arg@[("callback", PTCont ("contract", ret))];
        })
      ) di.signatures in
      ptl @ [Parse_tree.DInterface ({ di with signatures=signatures })]
    )
    | Parse_tree.DContract (dc) -> (
      let entries = List.map (fun de -> 
        (match de.ret with 
        | None -> de 
        | Some(ret) -> { de with 
          ret=None;
          arg=de.arg@[("callback", PTCont ("contract", ret))];
          (* TODO: traverse and check that there are no storage change *)
          pexpr=PEList([PEApply(PERef ("callback"), [de.pexpr])])
        })
      ) dc.entries in
      ptl @ [Parse_tree.DContract ({ dc with entries=entries })]
    )

    | _ -> ptl @ [dec]
  ) [] pt
