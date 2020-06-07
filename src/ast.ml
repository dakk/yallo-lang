type iden = string
[@@deriving show {with_path = false}]


(* 
  qua ci serve proprio un gadt, visto che abbiamo delle constraint sulla gerarchia dei type 
  ad esempio, non possiamo avere una map di map, le map devono avere tipo comparabile, etc
*)



(* let rec from_parse_tree pt (te: (string * atype) list) = () *)
(* 

let rec _from_parse_tree pt md = match pt with 
| [] -> 
  print_endline (show md);
  md
| Parse_tree.DType (id, t) :: pt' -> 
  if List.assoc_opt id md.type_map <> None then failwith ("Duplicate type declaration for: " ^ id)
  else _from_parse_tree pt' { md with type_map=((id, unroll_type t md.type_map)::md.type_map) }
| Parse_tree.DInterface (id, eid, sl) :: pt' -> _from_parse_tree pt' md
| Parse_tree.DContract (id, eid, iid, ul) :: pt' -> _from_parse_tree pt' md
| Parse_tree.DFunction (id, pl, rt, body) :: pt' -> (
  (* check name conflict *)
  try List.assoc id md.function_map; failwith ("Duplicate identifier: " ^ id) with | _ -> ();
  (* check parameter list *)
  List.iter (fun t -> (unroll_type (snd t) md.type_map) |> ignore) pl;
  (* check return type *)
  unroll_type rt md.type_map |> ignore;

  _from_parse_tree pt' { md with function_map=(id,())::md.function_map }
)
| invalid :: pt' -> failwith "Invalid declaration (this error can't happen)"

let from_parse_tree pt = _from_parse_tree pt ({ type_map= base_tenv; contract_map=[]; interface_map=[]; function_map=[] }) *)