open Ast

(* create a list assoc from name to basetype, raise if unknown type *)
let base_types = [
  ("address",  TBAddress);
  ("unit",     TBUnit);
  ("bool",     TBBool);
  ("mutez",    TBMutez);
  ("nat",      TBNat);
  ("int",      TBInt);
  ("timestamp",TBTimestamp);
  ("bytes",    TBBytes);
  ("string",   TBString);
  ("key",      TBKey);
  ("key_hash", TBKeyHash);
  ("chain_id", TBChainId);
  ("operation",TBOperation)
]


let get_type t te = try List.assoc t te with | _ -> failwith ("Unknown type: " ^ t)

(* transform a parsetree type to an ast type *)
let rec unroll_type t te  = match t with 
| Parse_tree.PTBase (id) -> get_type id te
| Parse_tree.PTTuple (tl) -> TCTuple (List.map (fun x -> unroll_type x te) tl)
| Parse_tree.PTRecord (tl) -> TCRecord (List.map (fun (a,b) -> (a, unroll_type b te)) tl)
| Parse_tree.PTCont (ctype, ptype) -> ( match ctype with
  | "list" -> TCList (unroll_type ptype te)
  | "set" -> TCSet (unroll_type ptype te)
  | "option" -> TCOption (unroll_type ptype te)
  | "map" -> (match ptype with 
    | Parse_tree.PTTuple ([a;b]) -> TCMap (unroll_type a te, unroll_type b te)
    | _ -> failwith ("Invalid type for map"))
  | "big_map" -> (match ptype with 
    | Parse_tree.PTTuple ([a;b]) -> TCBigMap (unroll_type a te, unroll_type b te)
    | _ -> failwith ("Invalid type for big_map"))
  | c -> failwith ("Invalid container type: " ^ c)
)
| Parse_tree.PTEnum (el) -> TEnum (el)

(* get a map of all type declarations *)
let rec _extract_type_map pt_new pt te = match pt with 
| [] -> (pt, te)
| Parse_tree.DType (id, t) :: pt' -> 
  if List.assoc_opt id te <> None then failwith ("Duplicate type declaration for: " ^ id)
  else _extract_type_map pt_new pt' ((id, unroll_type t te)::te)
| d :: pt' -> _extract_type_map (pt_new @ [d]) pt' te


let extract_type_map pt = _extract_type_map [] pt base_types



let rec typecheck (pt, te) = match pt with 
| _ -> ()