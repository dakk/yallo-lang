let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp


let sfmt = Format.str_formatter
let sget = Format.flush_str_formatter