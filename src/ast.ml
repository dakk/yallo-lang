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

open Parse_tree

let rec unroll_type t (te: tenv) = match t with 
| PTBase (id) -> List.assoc id te
| PTTuple (tl) -> TCTuple (List.map (fun x -> List.assoc x te) tl)
| PTRecord (tl) -> TCRecord (List.map (fun (a,b) -> (a, unroll_type b te)) tl)
| PTCont (ctype, ptype) -> ( match ctype with
  | "list" -> TCList (unroll_type ptype te)
  (* | "map" -> TCMap (unroll_type ptype te)
  | "big_map" -> TCBigMap (unroll_type ptype te) *)
  | "option" -> TCOption (unroll_type ptype te)
  | c -> failwith ("Invalid container type: " ^ c)
)
| PTEnum (el) -> TEnum (el)

let rec _from_parse_tree pt (te: tenv) (ce: cenv) (ie: ienv) = match pt with 
| [] -> TCTuple [TBBool; TBNat]
| DType (id, t) :: pt' -> _from_parse_tree pt' ((id, unroll_type t te)::te) ce ie
| DInterface (id, eid, sl) :: pt' -> _from_parse_tree pt' te ce ie
| DContract (id, eid, iid, ul) :: pt' -> _from_parse_tree pt' te ce ie
| DFunction (id, pl, rt, body) :: pt' -> _from_parse_tree pt' te ce ie
| invalid :: pt' -> failwith "Invalid declaration"

let from_parse_tree pt = _from_parse_tree pt (base_tenv) [] []