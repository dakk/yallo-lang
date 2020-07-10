let list_to_string l = List.fold_left (fun acc ll -> acc ^ ll) "" l
let pp_list2 l sep f = list_to_string (List.map (fun v -> f v ^ sep) l)
let pp_list l sep f = list_to_string (List.mapi (fun i v -> f v ^ (if i < (List.length l) - 1 then sep else "")) l)


(* new *)
let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_list3 sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp


let sfmt = Format.str_formatter
let sget = Format.flush_str_formatter