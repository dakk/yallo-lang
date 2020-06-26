exception SyntaxError of Loc.l option * string
exception ParsingError of Loc.l option * string
exception TypeError of Loc.l option * string
exception SymbolNotFound of Loc.l option * string
exception DuplicateSymbolError of Loc.l option * string
exception DeclarationError of Loc.l option * string
exception InvalidExpression of Loc.l option * string
exception ContractError of Loc.l option * string
exception APIError of Loc.l option * string
exception GenerateLigoError of Loc.l option * string
exception CompilerError of string

let red s = Printf.sprintf "\x1b[31m%s\x1b[0m" s
let yellow s = Printf.sprintf "\x1b[33m%s\x1b[0m" s

let pp_message p cc m err = match p with 
| Some (_, fn, l, c) -> Printf.sprintf "File %s, line %d, character %d: %s\n%s %s\n" 
  fn l c cc (if err then red "Error:" else yellow "Warning:") m
| None -> Printf.sprintf "File ?, line -1, character 0: %s\n%s %s\n" 
  cc (if err then red "Error:" else yellow "Warning:") m


let emit_warning p cc m = pp_message p cc m false |> print_endline