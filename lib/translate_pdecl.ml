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
    let et = transform_type dc.t e in
    let (t, exp) = transform_expr dc.v e in 

    let t = match (t, et) with
      | TBigMap (TAny, TAny), TBigMap(_, _) -> et 
      | TMap (TAny, TAny), TMap(_, _) -> et 
      | TList (TAny), TList (_) -> et
      | TSet (TAny), TSet (_) -> et
      | TOption (TAny), TOption (_) -> et
      | TString, TAddress -> TAddress
      | TString, TBytes -> TBytes 
      | TBytes, TString -> TString
      | TAddress, TString -> TString
      | a, b when a = b -> t
      | _, _ -> failwith ("Const '" ^ dc.id ^ "' expect to have type '" ^ show_ttype et ^ "', but type '" ^ show_ttype t ^ "' found")
    in

    transform p' { e with 
      symbols=(dc.id, Const)::e.symbols;
      consts=(dc.id, (t, exp))::e.consts;
    }

  (* functions *)
  | Parse_tree.DFunction (df) :: p' ->
    Env.assert_symbol_absence e df.id;


    transform p' e

  (* interface *)

  (* contracts *)

  | _ :: p' -> transform p' e
  | [] -> e