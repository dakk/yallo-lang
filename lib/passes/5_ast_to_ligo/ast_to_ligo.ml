open Ast
open Ast_ttype
open Ast_expr
open Helpers.Errors
open Parsing
open Format
open Helpers.Gen_utils

let temp_i = ref 0
let temp_v () = temp_i := !temp_i + 1; sprintf "a__%d" !temp_i
let temp_c () = sprintf "a__%d" !temp_i

let let_surround s = sprintf "let %s = %s in " (temp_v ()) s

let rec enum_index e i ii = match e with 
| [] -> failwith "Enum value not found"
| x::xe when x = i -> ii
| x::xe -> enum_index xe i (ii+1)


let rec pp_ltype (a: ttype) = match a with
| TUnit -> 
  sprintf "unit"

| TAddress -> 
  sprintf "address"

| TInt -> 
  sprintf "int"

| TChainId -> 
  sprintf "chain_id"

| TOperation -> 
  sprintf "operation"

| TNat -> 
  sprintf "nat"

| TMutez -> 
  sprintf "tez"

| TTimestamp -> 
  sprintf "timestamp"

| TBool -> 
  sprintf "bool"

| TSignature -> 
  sprintf "signature"

| TKeyHash -> 
  sprintf "key_hash"

| TKey -> 
  sprintf "key"

| TString -> 
  sprintf "string"

| TBytes -> 
  sprintf "bytes"

| TLambda (p, r) -> 
  sprintf "(%s -> %s)" 
  (pp_ltype p) 
  (pp_ltype r)

| TEnum (el) -> 
  sprintf "nat"

| TList (t) -> 
  sprintf "%s list" @@ pp_ltype t

| TSet (t) -> 
  sprintf "%s set" @@ pp_ltype t

| TMap (t, t') -> 
  sprintf "(%s, %s) map" 
  (pp_ltype t) 
  (pp_ltype t')

| TBigMap (t, t') -> 
  sprintf "(%s, %s) big_map" 
  (pp_ltype t) 
  (pp_ltype t')

| TOption (t) -> 
  sprintf "%s option" @@ pp_ltype t

| TRecord (l) -> "{ " ^ List.fold_left (fun acc (x, xt) -> acc ^ (if acc = "" then "" else "; ") ^ x ^ ": " ^ pp_ltype xt) "" l ^ " }"

| TTuple (tl) -> "(" ^ List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else " * ") ^ pp_ltype x) "" tl ^ ")"

| TContract (t) -> 
  sprintf "%s contract" @@ pp_ltype t

| _ -> raise @@ TypeError (None, sprintf "Type '%s' is not translable to ligo" (show_ttype a))



let rec pp_lexpr ((te,e): texpr) = 
  let pp_infix2 op a b = sprintf "(%s) %s (%s)" (pp_lexpr a) op (pp_lexpr b) in
  let pp_mergelist l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l) in
  let pp_mergelist2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l) in
  
match e with
| StorageEntry (i) -> 
  sprintf "((Tezos.self \"%%%s\"): %s)" i (pp_ltype te)

| Entrypoint((te2, ContractInstance((tt,e))), (TString, String(i))) -> 
  sprintf "match ((Tezos.get_entrypoint_opt \"%%%s\" (%s)): (%s) option) with | None -> (failwith \"Invalid entrypoint\": %s) | Some (ep) -> ep"
  i 
  (pp_lexpr (tt,e)) 
  (pp_ltype te) 
  (pp_ltype te)

(* 
| ContractInstance of expr 

| BuildContractCodeAndStorage of iden * expr list

| Entrypoint of expr * iden

| TezosAddressOfContract of expr
*)

| TezosContractOfAddress (ad) -> 
  sprintf "match (Tezos.get_contract_opt %s : unit contract option) with | None -> (failwith \"invalid contract\": unit contract) | Some(c) -> c"
  (pp_lexpr ad)

| TezosNow -> 
  sprintf "Tezos.now"

| TezosAmount -> 
  sprintf "Tezos.amount"

| TezosBalance -> 
  sprintf "Tezos.balance"

| TezosChainId -> 
  sprintf "Tezos.chain_id"

| TezosSource -> 
  sprintf "Tezos.source"

| TezosSender -> 
  sprintf "Tezos.sender"

| TezosSelf -> 
  sprintf "Tezos.self"

| TezosSetDelegate (a) -> 
  sprintf "Tezos.set_delegate (%s)" @@ pp_lexpr a

| TezosTransfer (ct, par, v) -> 
  sprintf "Tezos.transaction (%s) (%s) (%s)" 
  (pp_lexpr par) 
  (pp_lexpr v)
  (pp_lexpr ct)

| TezosSelfAddress -> 
  sprintf "Tezos.self_address"

| TezosCreateContract (cs, kho, mt) -> 
  sprintf "let create_contract: (key_hash option * tez * innerStorage) -> (operation * address) =\n
  \t[%%Michelson ( {| \n
  {\n
    UNPPAIIR ; \n
    CREATE_CONTRACT \n
    #include \"oc.tz\" \n
    ; \n
    PAIR\n
  }\n
  \t|} : (key_hash option * tez * innerStorage) -> (operation * address))] in\n
  \tcreate_contract (%s) (%s)\n"
  (pp_lexpr kho) 
  (pp_lexpr mt)

(*
| TezosImplicitAccount of expr
*)

| CryptoBlake2B (a) -> 
  sprintf "Crypto.blake2b (%s)" @@ pp_lexpr a

| CryptoCheckSignature (a,b,c) -> 
  sprintf "Crypto.check_signature (%s) (%s) (%s)" 
  (pp_lexpr a) 
  (pp_lexpr b)
  (pp_lexpr c)

| CryptoHashKey (a) -> 
  sprintf "Crypto.hask_key (%s)" @@ pp_lexpr a

| CryptoSha256 (a) -> 
  sprintf "Crypto.sha256 (%s)" @@ pp_lexpr a

| CryptoSha512 (a) -> 
  sprintf "Crypto.sha512 (%s)" @@ pp_lexpr a


| GlobalRef (id)
| LocalRef (id) -> 
  sprintf "%s" id

| StorageRef (id) -> 
  sprintf "s.%s" id


| None -> 
  sprintf "None"

| Unit -> 
  sprintf "unit"

| Bool (i) -> 
  sprintf "%b" i

| Nat (i) -> 
  sprintf "%dn" i

| Int (i) -> 
  sprintf "%d" i

| Mutez (i) -> 
  sprintf "%dmutez" i

| Address (a) -> 
  sprintf "(\"%s\": address)" a

| String (s) -> 
  sprintf "\"%s\"" s

| Key (a) -> 
  sprintf "(\"%s\": key)" a

| KeyHash (a) -> 
  sprintf "(\"%s\": key_hash)" a

| Some(a) -> 
  sprintf "Some (%s)" @@ pp_lexpr a

| Bytes (s) -> 
  sprintf "(\"%s\": bytes)" (Bytes.to_string s)

| Signature (s) -> 
  sprintf "(\"%s\": signature)" s

(* ChainId of int *)

| EnumValue (i) -> 
  (match te with | TEnum(e) -> sprintf "%dn" @@ enum_index e i 0)

| Typed (e, t) -> 
  sprintf "(%s: %s)" 
  (pp_lexpr e) 
  (pp_ltype t)
  
| List (el) -> 
  "[" ^ pp_list el "; " (fun e -> pp_lexpr e) ^ "]"

| Set (el) -> 
  "set [" ^ pp_list el "; " (fun e -> pp_lexpr e) ^ "]"

| Map (el) -> 
  "Map.literal [" ^
  pp_list el "; " (fun (a, b) -> "(" ^ pp_lexpr a ^ "), (" ^ pp_lexpr b ^ ")")
  ^ "]"

| BigMap (el) -> 
  "Big_map.literal [" ^
  pp_list el "; " (fun (a, b) -> "(" ^ pp_lexpr a ^ "), (" ^ pp_lexpr b ^ ")")
  ^ "]"

| Tuple (el) -> 
  "(" ^ pp_list el ", " (fun v -> pp_lexpr v) ^ ")"

| Lambda (il, e) -> 
  (
    if List.length il = 0 then 
      "( fun (override: unit) "
    else 
      "(fun (" ^ pp_list il ", " (fun (i,t) -> i) ^ (if List.length il > 0 then ": " else "") ^ pp_list il " * " (fun (i,t) -> pp_ltype t)  ^ ") "
  )
  ^ "-> " ^ pp_lexpr e ^ ")"
  (* "(fun (" ^ pp_list (List.split il) ", " (fun (i,t) -> i ^ ": " ^ pp_ltype t) ^ ") -> " ^ pp_lexpr e ^ ")" *)

| Record (il) -> 
  "{ " ^ pp_list il "; " (fun (i, e) -> sprintf "%s=%s" i @@ pp_lexpr e) ^ " }"
  
| RecordAccess (e, i) -> 
  sprintf "%s.%s" (pp_lexpr e) i

(* option *)
| OptionGetSome (oe) -> 
  sprintf "(match (%s) with | Some(v) -> v | None -> failwith \"Expect some value\")" (pp_lexpr oe)

| OptionIsSome(oe) -> 
  sprintf "(match (%s) with | Some(v) -> true | None -> false)" (pp_lexpr oe)

| OptionIsNone(oe) -> 
  sprintf "(match (%s) with | Some(v) -> true | None -> true)" (pp_lexpr oe)


(* map *)
| MapMem (mape, vkey) -> 
  sprintf "(match Map.find_opt (%s) %s with | None -> false | Some (v) -> true)"
  (pp_lexpr vkey)
  (pp_lexpr mape)

| MapFold (le, ll, initial) -> 
  sprintf "Map.fold (%s) (%s) (%s)" 
  (pp_lexpr ll)
  (pp_lexpr le)
  (pp_lexpr initial)

| MapMapWith (le, ll) -> 
  sprintf "Map.map (%s) (%s)" 
  (pp_lexpr ll) 
  (pp_lexpr le)

| MapSize (mape) -> 
  sprintf "Map.size (%s)" @@ pp_lexpr mape

| MapEmpty -> "Map.empty"

| MapGetForce (mape, vkey) -> 
  sprintf "(match Map.find_opt (%s) %s with | None -> ((failwith \"Key not present\"): %s) | Some (v) -> v)"
  (pp_lexpr vkey) 
  (pp_lexpr mape)
  (pp_ltype @@ match fst mape with | TMap (a, b) -> b)

| MapGet (mape, vkey, vdef) ->
  sprintf "(match Map.find_opt (%s) %s with | None -> %s | Some (v) -> v)"
  (pp_lexpr vkey)
  (pp_lexpr mape)
  (pp_lexpr vdef)

| MapGetOpt (mape, vkey) ->
  sprintf "Map.find_opt (%s) %s" 
  (pp_lexpr vkey)
  (pp_lexpr mape)

| MapUpdate (mape, vkey, vval) -> 
  sprintf "Map.update (%s) (Some (%s)) %s" 
  (pp_lexpr vkey) 
  (pp_lexpr vval) 
  (pp_lexpr mape)

| MapRemove (mape, vkey) -> 
  sprintf "Map.update (%s) (None) %s"
  (pp_lexpr vkey)
  (pp_lexpr mape)
  

(* bigmap *)
| BigMapEmpty -> 
  sprintf "Big_map.empty"

| BigMapMem (mape, vkey) -> 
  sprintf "(match Big_map.find_opt (%s) %s  with | None -> false | Some (v) -> true)"
  (pp_lexpr vkey)
  (pp_lexpr mape )

| BigMapGetForce (mape, vkey) -> 
  sprintf "(match Big_map.find_opt (%s) %s with | None -> ((failwith \"Key not present\"): %s) | Some (v) -> v)"
  (pp_lexpr vkey)
  (pp_lexpr mape)
  (show_ttype @@ match fst mape with | TMap (a, b) -> b)

| BigMapGet (mape, vkey, vdef) ->
  sprintf "(match Big_map.find_opt (%s) %s with | None -> %s | Some (v) -> v)"
  (pp_lexpr vkey)
  (pp_lexpr mape)
  (pp_lexpr vdef)

| BigMapGetOpt (mape, vkey) ->
  sprintf "Big_map.find_opt (%s) %s" 
  (pp_lexpr vkey) 
  (pp_lexpr mape)

| BigMapUpdate (mape, vkey, vval) -> 
  sprintf "Big_map.update (%s) (Some (%s)) %s"
  (pp_lexpr vkey)
  (pp_lexpr vval)
  (pp_lexpr mape)

| BigMapRemove (mape, vkey) -> 
  sprintf "Big_map.update (%s) (None) %s"
  (pp_lexpr vkey)
  (pp_lexpr mape)


(* set *)
| SetEmpty -> 
  sprintf "Set.empty"

| SetSize (le) ->
  sprintf "Set.size (%s)" @@ pp_lexpr le

| SetMem (le, lv) ->
  sprintf "Set.mem (%s) (%s)" 
  (pp_lexpr lv) 
  (pp_lexpr le)

| SetUpdate (se, sv, cc) -> 
  sprintf "if (%s) then (Set.add (%s) (%s)) else (Set.remove (%s) (%s))"
  (pp_lexpr cc)
  (pp_lexpr sv)
  (pp_lexpr se)
  (pp_lexpr sv)
  (pp_lexpr se)

(* list *)
| ListEmpty -> 
  sprintf "[]"

| ListMapWith (le, ll) -> 
  sprintf "List.map (%s) (%s)" 
  (pp_lexpr ll)
  (pp_lexpr le)

| ListPrepend (le, el) -> 
  sprintf "(%s) :: (%s)"
  (pp_lexpr el) 
  (pp_lexpr le)

| ListSize (le) ->
  sprintf "List.size (%s)" @@ pp_lexpr le

| ListFold (le, ll, initial) -> 
  sprintf "List.fold (%s) (%s) (%s)" 
  (pp_lexpr ll) 
  (pp_lexpr le)
  (pp_lexpr initial)

| ListFilter ((TList(lt), le), ll) ->
  sprintf "List.fold (fun (acc, e: %s * %s) -> if (%s)(e) then e::acc else acc) (%s) ([]: %s)"
  (pp_ltype (TList(lt)))
  (pp_ltype (lt))
  (pp_lexpr ll)
  (pp_lexpr (TList(lt), le))
  (pp_ltype (TList(lt)))

(*
| ListHead of expr
| ListTail of expr

(* string *)
| StringSlice of expr * expr * expr
*)
| StringSize (s) -> 
  sprintf "String.length (%s)" @@ pp_lexpr s

| StringConcat (a, b) -> 
  pp_infix2 "^" a b

(* bytes *)
| BytesPack(a) -> 
  sprintf "Bytes.pack (%s)" @@ pp_lexpr a

| BytesUnpack(a) -> 
  sprintf "Bytes.unpack (%s)" @@ pp_lexpr a

| BytesSize (s) -> 
  sprintf "Bytes.length (%s)" @@ pp_lexpr s

(* BytesSlice(a,b,c) *)

| BytesConcat (a, b) -> 
  pp_infix2 "^" a b


(* tuple *)
(*
| TupleFst of expr
| TupleSnd of expr
*)

(* aritmetic *) 
| Add(a,b) -> 
  pp_infix2 "+" a b
  
| Sub(a,b) -> 
  pp_infix2 "-" a b
  
| Mul(a,b) -> 
  pp_infix2 "*" a b
  
| Div(a,b) -> 
  pp_infix2 "/" a b
  
| Abs(a) -> 
  sprintf "abs(%s)" @@ pp_lexpr a

| ToInt(a) -> 
  sprintf "int(%s)" @@ pp_lexpr a

| IsNat(a) -> 
  sprintf "Michelson.is_nat(%s)" @@ pp_lexpr a

| Neg(a) -> 
  sprintf "- (%s)" @@ pp_lexpr a

| Mod (a, b) -> 
  pp_infix2 "mod" a b


(* bool *)
| Not(a) -> 
  sprintf "! (%s)" @@ pp_lexpr a

| And(a,b) -> 
  pp_infix2 "&&" a b
  
| Or(a,b) -> 
  pp_infix2 "||" a b

| Lt (a, b) -> 
  pp_infix2 "<" a b
  
| Lte (a, b) -> 
  pp_infix2 "<=" a b

| Gt (a, b) -> 
  pp_infix2 ">" a b

| Gte (a, b) -> 
  pp_infix2 ">=" a b

| Eq (a, b) -> 
  pp_infix2 "=" a b

| Neq (a, b) -> 
  pp_infix2 "<>" a b

| IfThenElse (c, a, b) -> 
  sprintf "(if %s then %s else %s)" 
  (pp_lexpr c) 
  (pp_lexpr a) 
  (pp_lexpr b)

| Apply((tce, Entrypoint((tci, ContractInstance(e)), i)), pp) ->
  sprintf "(Tezos.transaction (%s) (0mutez) (%s))"
  (pp_lexpr pp)
  (pp_lexpr (tce, Entrypoint((tci, ContractInstance(e)), i)))

| Apply(lam, par) -> 
  sprintf "%s (%s)" 
  (pp_lexpr lam) 
  (pp_lexpr par)

| MatchWith (e, el) -> 
  let rec rr el = (match el with 
  | [] -> ""
  | (e', te')::((_, CaseDefault), tee')::el' -> 
    sprintf "if %s = (%s) then (%s: %s) else (%s: %s)"
    "tmatchwithtemp"
    (pp_lexpr e')
    (pp_lexpr te')
    (pp_ltype te)
    (pp_lexpr tee')
    (pp_ltype te)

  | (e', te')::elle::el' -> 
    sprintf "if %s = (%s) then (%s: %s) else %s" 
    "tmatchwithtemp"
    (pp_lexpr e')
    (pp_lexpr te')
    (pp_ltype te)
    (rr @@ elle::el')

  | (e', te')::[] -> 
    sprintf "if %s = (%s) then (%s: %s) " 
    "tmatchwithtemp"
    (pp_lexpr e')
    (pp_lexpr te')
    (pp_ltype te)

  ) in 
  sprintf "let %s = %s in %s" 
  "tmatchwithtemp"
  (pp_lexpr e) 
  (rr el)

  
| FailIfMessage (e, m) -> 
  sprintf "if (%s) then failwith (%s) else ()"
  (pp_lexpr e)
  (pp_lexpr m)

| FailIf (e) -> 
  sprintf "if (%s) then failwith \"Assertion\" else ()"
  (pp_lexpr e)

| Fail (e) -> 
  sprintf "failwith (%s)" @@ pp_lexpr e

| Assert (e) -> 
  sprintf "if (%s) then () else (failwith \"Assertion\")" @@ pp_lexpr e

| Copy (e) -> 
  sprintf "(%s)" @@ pp_lexpr e
     
| Let (id, tt, e) -> 
  sprintf "let %s: %s = %s in " 
  id 
  (pp_ltype tt) 
  (pp_lexpr e)

| LetIn (id, tt, e, e2) -> 
  sprintf "let %s: %s = %s in %s " 
  id 
  (pp_ltype tt) 
  (pp_lexpr e) 
  (pp_lexpr e2)

| LetTuple (il, e) -> 
  "let (" ^ pp_list il ", " (fun (id, t) -> id) ^ ") = " ^ pp_lexpr e ^ " in "

| LetTupleIn (il, e, e2) -> 
  "let (" ^ pp_list il ", " (fun (id, t) -> id) ^ ") = " ^ pp_lexpr e ^ " in " ^ pp_lexpr e2

| SAssign (i, e) -> 
  sprintf "let s = { s with %s=%s } in " 
  i (pp_lexpr e)

| SRecAssign (i, ii, expr) -> 
  sprintf "let s = { s with %s= {s.%s with %s=%s} } in " 
  i i ii (pp_lexpr expr)

| Seq(a, b) -> 
  (match a with 
  | (TUnit, LetTuple(_, _)) 
  | (TUnit, LetTupleIn(_, _, _))
  | (TUnit, LetIn(_, _, _, _))
  | (TUnit, Let(_, _, _)) 
  | (TUnit, SAssign(_, _)) 
  | (TUnit, SRecAssign(_, _, _)) -> pp_lexpr a
  | (TUnit, _) -> let_surround (pp_lexpr a)
  | _ -> pp_lexpr a)
  ^ "\n" ^
  (match b with 
  | (tl, List(e)) -> sprintf "(%s: operation list)" @@ pp_lexpr (tl, List(e))
  | _ -> pp_lexpr b)


(* | _ -> failwith @@ "Unable to generate ligo code for expression " ^ show_expr e *)
(* | _ -> "<<translation not handled: " ^ show_expr e ^ ">>" *)




let generate_ligo_code (ast: t) (contract: string) = 
  let ce = List.assoc contract ast.contracts in

  (* dump const *)
  let consts = (List.map (fun (i, (t,e)) -> 
    sprintf "let %s = %s\n" 
    i 
    (pp_lexpr (t,e))
  ) ast.consts) in 

  (* generate the storage record *)
  let str =
    if List.length ce.fields = 0 then 
      "type storage = unit\n\n"
    else 
      "type storage = {\n" ^
      pp_list ce.fields ";\n" (fun (i, t) -> "  " ^ i ^ ": " ^ pp_ltype t) ^
      ";\n}\n\n"
  in 

  (* generate the action variant *)
  let act = 
    if List.length ce.entries = 0 then ""
    else (
      "type action = " ^
      (list_to_string (List.map (fun e -> 
        "| " ^ String.capitalize_ascii e.id ^ 
          (if List.length e.arg > 0 then " of " ^ pp_list e.arg " * " (fun (ii, it) -> pp_ltype it)
          else " of unit")
        ) ce.entries
      ) )
      ^"\n\n"
  ) in 

  (* write entries *)
  let entrs = 
    List.map (fun e -> 
    "let " ^ e.id ^ " (" ^
      list_to_string (List.mapi (fun i (ii,it) -> ii ^ ", ") e.arg) ^
      "s: " ^ pp_list2 e.arg " * " (fun (ii, it) -> pp_ltype it) ^
      "storage) = \n" ^ pp_lexpr e.expr ^ ", (s: storage)\n\n"
  ) ce.entries in

  (* write the main *)
  let main =
    if List.length ce.entries = 0 then (
      "let main(a, s: unit * storage): (operation list * storage) = " ^
      "([]: operation list), s"
    ) else (
      "let main(a, s: action * storage): (operation list * storage) = " ^
      "match a with" ^
      (List.map (fun e -> 
        "| " ^ String.capitalize_ascii e.id ^ " (arg) -> " ^ 
        if List.length e.arg > 0 then 
          "let (" ^ pp_list e.arg ", " (fun (ii, it) -> ii)
          ^ ") = arg in " ^ e.id ^ "(" ^ pp_list2 e.arg ", " (fun (ii, it) -> ii) ^ "s)"
        else 
          e.id ^ "(s)") ce.entries |> list_to_string) 
    )
  in
  (consts@[str]@[act]@entrs@[main]) |> list_to_string


let generate_ligo (ast: t) (contract: string) = 
  generate_ligo_code ast contract