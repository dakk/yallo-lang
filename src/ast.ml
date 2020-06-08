open Parse_tree 

type iden = string [@@deriving show {with_path = false}]

type atype = Typecheck.atype [@@deriving show {with_path = false}]
let unroll_type = Typecheck.unroll_type

type t = {
  (* we also need interfaces, for calling sugar *)
  constants: (iden * atype * unit) list;
  functions: (iden * (iden * atype) list * atype * unit) list;
  storage: (iden * atype * unit) list;
  entrypoints: (iden * (iden * atype) list * unit) list
} [@@deriving show {with_path = false}]

let rec _from_parse_tree pt tenv s = match pt with
| [] -> s
| (DConst (id, t, value))::pt' ->  
  (* todo: value! *)
  let s' = { s with constants=(id, unroll_type t tenv, ())::s.constants } in
  _from_parse_tree pt' tenv s'
| (DFunction (id,pl,rt,body))::pt' -> 
  let s' = { s with 
    (* todo: body! *)
    functions=(id, List.map (fun (n,p) -> n, unroll_type p tenv) pl, unroll_type rt tenv, ())::s.functions
  } in
  _from_parse_tree pt' tenv s'
| (DContract (id,_,sl,el))::pt' -> 
  let s' = { s with 
  (* todo: value! *)
    storage=List.map (fun (id, ptype, value) -> (id, unroll_type ptype tenv, ())) sl;
    entrypoints=List.map (fun (id, pl, body) -> (id, List.map (fun (n,p) -> n, (unroll_type p tenv)) pl, body)) el;
  } in
  _from_parse_tree pt' tenv s'
| _ -> failwith "Invalid"

let from_parse_tree ((t: Typecheck.t), pt) = _from_parse_tree pt t.types { constants= []; functions= []; storage= []; entrypoints= [] }
