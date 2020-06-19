
type code = 
| CStr of code list
| Str of string
| Surround of string * string * code 
| Merge of string * code list
| Empty
| Level of code list


let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let merge_list2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l)
let merge_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)

let code_to_string c = 
  let rec cts c l = match c with 
  | [] -> ""
  | Empty::cl' -> "\n" ^ cts cl' l
  | Merge(sep, ll)::cl' -> merge_list ll sep (fun x -> cts [x] l)
  | Surround (a,b,e)::cl' -> a ^ (cts [e] l) ^ b ^ "\n" ^ cts cl' l
  | CStr(sl)::cl' -> (String.make (if l = 0 then 0 else 2*(l-1)) ' ') ^ cts sl l ^ "\n" ^ cts cl' l
  | Str(s)::cl' -> (String.make (if l = 0 then 0 else 2*(l-1)) ' ') ^ s ^ "\n" ^ cts cl' l
  | Level(cl)::cl' -> cts cl (l+1) ^ cts cl' l
  in cts [c] 0