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
    let (st, se) = transform_expr df.exp e @@ List.map (fun (i,t) -> i, Local(t)) pars in 
    if st <> rettype then failwith @@ "Function return type mismatch, got: '" ^ show_ttype st ^ "', expect: '" ^ show_ttype rettype ^ "'";

    transform p' { e with 
      symbols=(df.id, Const)::e.symbols;
      consts=(df.id, (TLambda(TTuple(snd @@ List.split pars), rettype), Lambda(pars, se)))::e.consts;
    }

  (* interface *)
  | Parse_tree.DInterface (di) :: p' -> 
    Env.assert_symbol_absence e di.id;

    (* if extends, get the list of entries *)
    let ex = (match di.extends with | None -> [] 
    | Some (i) -> (match List.assoc_opt i e.ifaces with 
      | None -> failwith @@ "Interface " ^ di.id ^ " extends an unknown interface " ^ i 
      | Some(el) -> el)
    ) in 

    (* extract signatures *)
    let el = List.map (fun (x, xl) -> 
      x, List.map (fun (xi, xx) -> xi, transform_type xx e) xl
    ) di.signatures in

    (* check for duplicates *)
    let rec dup_fail lst b = match lst with
      | [] -> ()
      | (hdi, hdl)::tl -> 
        if (List.exists (fun (x,l) -> x = hdi) b)
        then failwith @@ "Duplicate identifier " ^ hdi ^ " in interface " ^ di.id
        else dup_fail tl ((hdi, hdl)::b) 
    in dup_fail (el @ ex) [];

    transform p' { e with 
      symbols=(di.id, Interface)::e.symbols;
      ifaces=(di.id, el)::e.ifaces; 
    }

  (* contracts *)
  | Parse_tree.DContract (dc) :: p' -> 
    Env.assert_symbol_absence e dc.id;
    let flds = List.map (fun (i,t) -> i, transform_type t e) dc.fields in

    (* if implements, get the list of entries *)
    let to_implement = (match dc.implements with | None -> [] 
      | Some (i) -> (match List.assoc_opt i e.ifaces with 
        | None -> failwith @@ "Contract " ^ dc.id ^ " extends an unknown interface " ^ i 
        | Some(el) -> el)
    ) in 

    (* handle constructor *)
    let ctor = (match dc.constructor with | None -> [], []
      | Some (par, ass) -> 
        let par' = List.map (fun (i, a) -> i, transform_type a e) par in
        par',
        List.map (fun (i, a) -> i, snd @@ transform_expr a e @@ List.map (fun (i,t) -> i, Local(t)) par') ass
    ) in
    if (snd ctor) <> [] && List.length (flds) <> List.length (snd ctor) then
      failwith "Constructor left some fields uninitialized";
    
    (* entry list *)
    let el = List.map (fun (i, p, ex) -> 
      let p' = List.map (fun (ii, pp) -> ii, transform_type pp e) p in 
      let flds_bind = List.map (fun (i,t) -> i, Storage(t)) flds in
      let p_bind = List.map (fun (i,t) -> i, Local(t)) p' in
      let tt, ee = transform_expr ex e (p_bind @ flds_bind) in 
      if tt<>TList(TOperation) && tt<>TList(TAny) then failwith @@ "Entry " ^ i ^ " of contract " ^ dc.id ^ " does not evalute to an operation list";
      (i, (p', ee))
    ) dc.entries in

    (* assert no duplicated entry *)
    let rec dup_fail lst b = match lst with
      | [] -> ()
      | (hdi, hdl)::tl -> 
        if (List.exists (fun (x,l) -> x = hdi) b)
        then failwith @@ "Duplicate entry " ^ hdi ^ " in contract " ^ dc.id
        else dup_fail tl ((hdi, hdl)::b) 
    in dup_fail el [];

    (* assert all to_implement are implemented *)
    List.iter (fun (i, _) ->
      if List.assoc_opt i @@ el = None then failwith @@ "Contract " ^ dc.id ^ " does not implement " ^ i;
      ()
    ) to_implement;

    let el = List.map (fun (x, (a,b)) -> x,a,b) el in

    transform p' { e with 
      symbols=(dc.id, Contract)::e.symbols;
      contracts=(dc.id, (ctor, el))::e.contracts;
    }

  | _ :: p' -> transform p' e
  | [] -> e