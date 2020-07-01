{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let letter_up = ['A'-'Z']
let letter_dw = ['a'-'z']
let letter = letter_up | letter_dw

let annot = '%' letter (letter | digit | '_') *
let string = letter (letter | digit | '_') *

rule token = parse 
  (* | newline         { Lexing.new_line lexbuf; token lexbuf }
  | blank+          { token lexbuf } *)

  | "paramter"      { PARAMETER }
  | "storage"       { STORAGE }
	| "code"					{ CODE }

  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "("             { LPAR }
  | ")"             { RPAR }
  | annot as i      { ANNOT (String.sub i 1 @@ (String.length i) - 1) }
  | string as i     { STRING (i) }

  | eof             { EOF }
  | _ as c          { failwith (Format.sprintf "Invalid string starting with %C" c) }

