type iden = string
[@@deriving show {with_path = false}]

type atype = 
| TBBool
| TBAddress
| TBInt
| TBNat
| TBMutez
| TBTimestamp
| TBKey
| TBOperation
| TBString
| TBBytes
| TBUnit
| TBChainId
| TBKeyHash
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

type itype = iden option * (iden * atype list) list (* extend type * (ident * params) list *)
[@@deriving show {with_path = false}]

type ctype = iden option * iden option * (iden * atype) list * (iden * atype list) list (* extend type * interface type * storage types * (ident * params) list *)
[@@deriving show {with_path = false}]

type ftype = atype list * atype (* param list * return type *)
[@@deriving show {with_path = false}]

type tenv = (string * atype) list [@@deriving show {with_path = false}]
type ienv = (string * itype) list [@@deriving show {with_path = false}]
type fenv = (string * ftype) list [@@deriving show {with_path = false}]
type cenv = (string * ctype) list [@@deriving show {with_path = false}]

type t = {
  types: tenv;
  interfaces: ienv;
  contracts: cenv;
  functions: fenv;
  symbols: string list;
}
[@@deriving show {with_path = false}]

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

let empty = {
  { types= []; interfaces= []; contracts= []; functions= []; symbols= []; }
  with
  types=base_types; symbols=(fst @@ List.split base_types)
}


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



let extract_type a (id, t) = 
  if List.mem id a.symbols then failwith ("Duplicate declaration for: " ^ id) else ();
  { a with types=(id, unroll_type t a.types)::(a.types); symbols=id::a.symbols }


let extract_function a (id, par, ret) = 
  if List.mem id a.symbols then failwith ("Duplicate declaration for: " ^ id) else ();
  let tf = (
    List.map (fun p -> unroll_type p (a.types)) (snd @@ List.split par),
    unroll_type ret (a.types)
  ) in
  { a with functions=(id, tf)::(a.functions); symbols=id::a.symbols }

let extract_interface a (id, ex, sl) = 
  if List.mem id a.symbols then failwith ("Duplicate declaration for: " ^ id) else ();
  let ex_i = (match ex with 
  | None -> None
  | Some(e) -> if List.assoc_opt e a.interfaces = None then failwith ("Interface " ^ id ^ " extends an unknown interface: " ^ e) else Some (e)) in
  let te = List.fold_left (fun acc (n, t, _) -> 
    if List.assoc_opt n acc <> None then failwith ("Duplicate entrypoint: " ^ n) else (); 
    (n, List.map (fun (i,p) -> unroll_type p (a.types)) t)::acc
  ) [] sl in
  let ti = (ex_i, te) in 
  { a with interfaces=(id, ti)::(a.interfaces); symbols=id::a.symbols }


let extract_contract a (id, ex, im, fl, el) = 
  if List.mem id a.symbols then failwith ("Duplicate declaration for: " ^ id) else ();
  let ex_i = (match ex with 
    | None -> None
    | Some(e) -> if List.assoc_opt e a.contracts = None then failwith ("Contract " ^ id ^ " extends an unknown contract: " ^ e) else Some (e)) in
  let im_i = (match im with 
    | None -> None
    | Some(e) -> if List.assoc_opt e a.interfaces = None then failwith ("Contract " ^ id ^ " implements an unknown interface: " ^ e) else Some (e)) in
  let tf = List.fold_left (fun (acc: (string * atype) list) (n, t, _) -> 
    if List.assoc_opt n acc <> None then failwith ("Duplicate field: " ^ n) else ();
    (n, unroll_type t a.types)::acc
  ) [] fl in
  let te = List.fold_left (fun acc (n, t, _) -> 
    if List.assoc_opt n acc <> None then failwith ("Duplicate entrypoint: " ^ n) else (); 
    (n, List.map (fun (i,p) -> unroll_type p (a.types)) t)::acc
  ) [] el in
  let tc = ( ex_i, im_i, tf, te ) in 
  { a with contracts=(id, tc)::(a.contracts); symbols=id::a.symbols }
  

(* get a map of all type declarations *)
let rec extract_types pt a = match pt with 
| [] -> a
| Parse_tree.DContract (id, ex, im, fl, el) :: pt' -> extract_types pt' @@ extract_contract a (id, ex, im, fl, el)
| Parse_tree.DInterface (id, ex, sl) :: pt' -> extract_types pt' @@ extract_interface a (id, ex, sl)
| Parse_tree.DFunction (id, par, ret, _) :: pt' -> extract_types pt' @@ extract_function a (id, par, ret)
| Parse_tree.DType (id, t) :: pt' -> extract_types pt' @@ extract_type a (id, t)
| d :: pt' -> extract_types pt' a



(* check that contracts implements all the interfaces signatures *)
let rec check_contracts_implement_extend (a, pt) = 
  let rec unroll_signatures (eopt, sl) = match eopt with 
  | None -> sl
  | Some (en) -> sl @ unroll_signatures @@ List.assoc en a.interfaces 
  in
  let rec unroll_contracts (eopt, imopt, fl, el) = match eopt with 
  | None -> el
  | Some (en) -> el @ unroll_contracts @@ List.assoc en a.contracts 
  in
  List.iter (fun (id, (ex, im, fl, el)) -> 
    let sl = unroll_signatures (im, []) in 
    let csl = unroll_contracts (ex, None, [], el) in 
    List.iter (fun (sname, s) -> 
      match List.assoc_opt sname csl with 
      | None -> failwith ("Contract " ^ id ^ " does not implement: " ^ sname) 
      | Some (si) when s <> si -> failwith ("Contract " ^ id ^ " implements entrypoint " ^ sname ^ " using a wrong signature") 
      | _ -> ()
    ) sl 
  ) a.contracts;
  (a, pt)
;;

