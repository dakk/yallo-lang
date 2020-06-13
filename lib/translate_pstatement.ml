open Ast_env
open Ast_statement
open Ast_ttype
open Translate_ptype
open Translate_pexpr

let rec transform_statements (pl: Parse_tree.pstatement list) (env': Env.t) : (statement list) = 
  match pl with 
  | [] -> []
  (* 
  | Parse_tree.PSVarAssignTuple (tu, e)::pl' ->
  | Parse_tree.PSVarAssign (i, t, e)::pl' -> 
  | Parse_tree.PSAssign (lo, e)::pl'
  | Parse_tree.PSRecAssign (lo, i, e)::pl' ->
  | Parse_tree.PSCall (lo, i, el)::pl' -> 
  *)

  | Parse_tree.PSVar (i, t)::pl' -> 
    Env.assert_symbol_absence env' i;
    let t' = transform_type t env' in
    (DeclareVar(i, t'))::transform_statements pl' { env' with 
      scope_stack=(Scope.add_var i t' (List.hd env'.scope_stack))::(List.tl env'.scope_stack)
    }

  | Parse_tree.PSConst (i, t, e)::pl' -> 
    Env.assert_symbol_absence env' i;
    let t' = transform_type t env' in
    let (tt, ee) = transform_expr e env' in
    if tt <> t' then failwith @@ "Assigning wrong type to a const; got: '" ^ show_ttype tt ^ "' expect: '" ^ show_ttype t' ^ "'";

    (DeclareConst(i, tt, ee))::transform_statements pl' { env' with 
      scope_stack=(Scope.add_const i t' (List.hd env'.scope_stack))::(List.tl env'.scope_stack)
    }

  | Parse_tree.PSIfThenElse (e, s1, s2)::pl' -> 
    let (tt, ee) = transform_expr e env' in 
    if tt <> TBool then failwith "If condition must be a bool expression";
    let s1' = transform_statements s1 env' in 
    let s2' = transform_statements s2 env' in 
    (If(ee, s1', Some(s2')))::transform_statements pl' env'

  | Parse_tree.PSIfThen (e, sl)::pl' -> 
    let (tt, ee) = transform_expr e env' in 
    if tt <> TBool then failwith "If condition must be a bool expression";
    let sl' = transform_statements sl env' in 
    (If(ee, sl', None))::transform_statements pl' env'

  | Parse_tree.PSCallBuiltin (i, el)::pl' ->
    let el' = List.map (fun x -> transform_expr x env') el in 
    
    (* check if we are calling a standard function *)
    let cmd = (match i with 
      | "assert" -> (match el' with 
        | [(TBool, e1)] -> Some (Assert(e1))
        | _ -> failwith "Invalid arguments for assert"
      )
      | "failif" -> (match el' with 
        | [(TBool, e1)] -> Some (FailIf(e1))
        | [(TBool, e1); (TString, e2)] -> Some (FailIfMessage(e1, e2))
        | _ -> failwith "Invalid arguments for failif"
      )
      | _ -> None
    ) in if cmd <> None then (Option.get cmd)::transform_statements pl' env' else 
      failwith @@ "Unknown builting function " ^ i    

  | Parse_tree.PSSkip::pl' ->
    Skip::transform_statements pl' env'

  | Parse_tree.PSReturn (e)::pl' -> 
    let (tt, ee) = transform_expr e env' in 
    let scp = List.hd env'.scope_stack in 
    (match scp.stype, tt, scp.rettype with
    | Function, tt, tr when tt = tr -> ()
    | Function, tt, tr when tt <> tr -> failwith @@ "This function must return '" ^ show_ttype tr ^ "' but '" ^ show_ttype tt ^ "' found"
    );
    (Return (tt, ee))::transform_statements pl' env'
