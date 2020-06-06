type iden = string
[@@deriving show {with_path = false}]


(* 
  qua ci serve proprio un gadt, visto che abbiamo delle constraint sulla gerarchia dei type 
  ad esempio, non possiamo avere una map di map, le map devono avere tipo comparabile, etc
*)

type atype = 
| TBBool
| TBAddress
| TBInt
| TBNat
| TBTez
| TBTimestamp
| TBKey
| TBOperation
| TBString
| TBBytes
| TBUnit
| TBOption of atype
| TEnum of string list
| TCList of atype
| TCSet of atype
| TCMap of atype * atype
| TCBigMap of atype * atype
| TCTuple of atype list
| TCOption of atype
| TCRecord of (iden * atype) list
[@@deriving show {with_path = false}]


type t = 
  atype
[@@deriving show {with_path = false}]

type ienv = string list
type cenv = string list

(* create a list assoc from name to basetype, raise if unknown type *)
type tenv = (string * atype) list
[@@deriving show {with_path = false}]

let base_tenv = [
  ("address",  TBAddress);
  ("unit",     TBUnit);
  ("bool",     TBBool);
  ("tez",      TBTez);
  ("nat",      TBNat);
  ("int",      TBInt);
  ("timestamp",TBTimestamp);
  ("bytes",    TBBytes);
  ("string",   TBString);
  ("key",      TBKey);
  ("operation",TBOperation)
]


let get_type t te = try List.assoc t te with | _ -> failwith ("Unknown type: " ^ t)

(* transform a parsetree type to an ast type *)
let rec unroll_type t (te: tenv) = match t with 
| Parse_tree.PTBase (id) -> get_type id te
| Parse_tree.PTTuple (tl) -> TCTuple (List.map (fun x -> unroll_type x te) tl)
| Parse_tree.PTRecord (tl) -> TCRecord (List.map (fun (a,b) -> (a, unroll_type b te)) tl)
| Parse_tree.PTCont (ctype, ptype) -> ( match ctype with
  | "list" -> TCList (unroll_type ptype te)
  | "set" -> TCSet (unroll_type ptype te)
  | "map" -> (match ptype with 
    | Parse_tree.PTTuple ([a;b]) -> TCMap (unroll_type a te, unroll_type b te)
    | _ -> failwith ("Invalid type for map"))
  | "big_map" -> (match ptype with 
    | Parse_tree.PTTuple ([a;b]) -> TCBigMap (unroll_type a te, unroll_type b te)
    | _ -> failwith ("Invalid type for big_map"))
  | "option" -> TCOption (unroll_type ptype te)
  | c -> failwith ("Invalid container type: " ^ c)
)
| Parse_tree.PTEnum (el) -> TEnum (el)

let rec _from_parse_tree pt (te: tenv) (ce: cenv) (ie: ienv) = match pt with 
| [] -> 
  print_endline (show_tenv te);
  TCTuple [TBBool; TBNat]
| Parse_tree.DType (id, t) :: pt' -> 
  if List.assoc_opt id te <> None then failwith ("Duplicate type declaration for: " ^ id)
  else _from_parse_tree pt' ((id, unroll_type t te)::te) ce ie
| Parse_tree.DInterface (id, eid, sl) :: pt' -> _from_parse_tree pt' te ce ie
| Parse_tree.DContract (id, eid, iid, ul) :: pt' -> _from_parse_tree pt' te ce ie
| Parse_tree.DFunction (id, pl, rt, body) :: pt' -> _from_parse_tree pt' te ce ie
| invalid :: pt' -> failwith "Invalid declaration"

let from_parse_tree pt = _from_parse_tree pt (base_tenv) [] []