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
| TBString
| TBBytes
| TBUnit
| TBOption of atype
| TCList of atype
| TCMap of atype * atype
| TCBigMap of atype * atype
| TCTuple of atype list
| TCRecord of (iden * atype) list
[@@deriving show {with_path = false}]


type t = atype
[@@deriving show {with_path = false}]

let from_parse_tree pt = TCTuple [TBBool; TBNat]