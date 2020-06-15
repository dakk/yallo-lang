open Ast_ttype
open Ast_env

(* transform a pttype to a ttype *)
let rec transform_type (pt: Parse_tree.ptype) (e: Env.t): ttype = match pt with 
| Parse_tree.PTBuiltin (tn) -> 
  (match Env.get_type_opt tn e with 
  | None -> failwith ("Undefined type '" ^ tn ^ "'")
  | Some (t) -> t)
| Parse_tree.PTTuple (tl) -> 
  TTuple (List.map (fun tt -> transform_type tt e) tl)
| Parse_tree.PTCont (c, tt) -> (
  let assert_cmp_key a = if not (attributes a).cmp then 
    failwith ("Type '" ^ show_ttype a ^ "' is not comparable and cannot be used as key of " ^ c)
    else () 
  in
  let tt' = transform_type tt e in
  match c with 
  | "list" -> TList (tt') 
  | "map" -> (match tt' with 
    | TTuple (a::b::[]) -> assert_cmp_key a; TMap (a, b)
    | _ -> failwith ("Type for map should be a tuple ('a, 'b'), got: " ^ show_ttype tt'))
  | "big_map" -> (match tt' with 
    | TTuple (a::b::[]) -> assert_cmp_key a; TBigMap (a, b)
    | _ -> failwith ("Type for big_map should be a tuple ('a', 'b'), got: " ^ show_ttype tt'))
  | "set" -> assert_cmp_key tt'; TSet (tt')
  | "option" -> TOption (tt')
  | "contract" -> TContract (tt')
  | c -> failwith ("Invalid container type '" ^ c ^ "'")
)
| Parse_tree.PTRecord (el) -> TRecord (List.map (fun (n, tt) -> n, transform_type tt e) el)
| Parse_tree.PTEnum (e) -> TEnum (e)
| Parse_tree.PTLambda (p, r) -> TLambda (transform_type p e, transform_type r e)

