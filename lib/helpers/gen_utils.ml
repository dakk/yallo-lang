let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_capit fmt i =
  Format.fprintf fmt "%s" (String.capitalize_ascii i)

let sfmt = Format.str_formatter
let sget = Format.flush_str_formatter



let temp_i = ref 0
let temp_v () = temp_i := !temp_i + 1; Format.sprintf "a__%d" !temp_i
let temp_c () = Format.sprintf "a__%d" !temp_i
let reset_temp () = temp_i := 1