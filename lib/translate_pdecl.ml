open Ast_env
open Ast_ttype
open Translate_pexpr
open Translate_ptype

let rec transform (p: Parse_tree.t) (e: Env.t): Env.t = 
  match p with 
  (* type definition *)
  | Parse_tree.DType (dt) :: p' -> 
    Env.assert_symbol_absence e dt.id;

    transform p' { e with 
      symbols=(dt.id, Type)::e.symbols;
      types=(dt.id, transform_type dt.t e)::e.types;
    }

  (* global const *)
  | Parse_tree.DConst (dc) :: p' -> 
    Env.assert_symbol_absence e dc.id;

    let (t, exp) = (match dc.t with
    | None -> 
      let (t, exp) = transform_expr dc.v e [] in 
      (match (t) with
        | TBigMap (TAny, TAny)
        | TMap (TAny, TAny)
        | TList (TAny)
        | TSet (TAny)
        | TOption (TAny) -> failwith @@ "Unable to infer type of const '" ^ dc.id ^ "'"
        | _ -> (t, exp))
    | Some(ptt) ->
      let et = transform_type ptt e in
      let (t, exp) = transform_expr dc.v e [] in 

      let t = match (t, et) with
        | TBigMap (TAny, TAny), TBigMap(_, _) -> et 
        | TMap (TAny, TAny), TMap(_, _) -> et 
        | TList (TAny), TList (_) -> et
        | TSet (TAny), TSet (_) -> et
        | TOption (TAny), TOption (_) -> et
        | a, b when a = b -> t
        | _, _ -> failwith ("Const '" ^ dc.id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
      in t, exp)
    in transform p' { e with 
      symbols=(dc.id, Const)::e.symbols;
      consts=(dc.id, (t, exp))::e.consts;
    }

  (* functions *)
  | Parse_tree.DFunction (df) :: p' ->
    Env.assert_symbol_absence e df.id;
    let rettype = transform_type df.rettype e in 
    let pars = List.map (fun (i, t) -> (i, transform_type t e)) df.params in 
    let nscope = Scope.add_consts pars @@ Scope.empty Function in     
    let nscope = { nscope with rettype = transform_type df.rettype e } in
    let (st, se) = transform_expr df.exp (Env.push_scope e nscope) [] in 
    if st <> rettype then failwith @@ "Function return type mismatch, got: '" ^ show_ttype st ^ "', expect: '" ^ show_ttype rettype ^ "'";

    transform p' { e with 
      symbols=(df.id, Const)::e.symbols;
      consts=(df.id, (TLambda(TTuple(snd @@ List.split pars), rettype), Lambda(pars, se)))::e.consts;
    }

    (* { e with functions=(df.id, ) *)

  (* interface *)

  (* contracts *)

  | _ :: p' -> transform p' e
  | [] -> e