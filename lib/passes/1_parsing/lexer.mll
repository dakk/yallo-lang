{
  open Parser
  open Lexing
	
	exception SyntaxError2 of string

  let reserved = [ 
    "interface"; 
    "contract"; 
    "entry"; 
    "extends"; 
    "implements"; 
    "function"; 
    "type"; 
    "enum";
    "record";
		"this";
		"const";
		"var";
		"list";
		"map";
		"set";
		"option";
		"let";
		"in";
		"true";
		"false";
		"and";
		"or";
		"not";
		"constructor";
		"using";
  ]


	let next_line lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{ pos with pos_bol = lexbuf.lex_curr_pos;
								pos_lnum = pos.pos_lnum + 1
			}

}

let digit = ['0'-'9']
let letter_up = ['A'-'Z']
let letter_dw = ['a'-'z']
let letter = letter_up | letter_dw

let ident = letter (letter | digit | '_')*
let modifier = "@" ident
let nat = digit digit* "n"
let int = digit digit*
let mtz = digit digit* "mtz"
let tz = digit digit* ('.' digit digit*)? "tz"

let hex_digit = ['a'-'f'] | ['A' - 'F'] | ['0' - '9']

let blank = [' ' '\t' '\r']
let newline = '\n'
let quote = '"'
let string = quote (letter | digit | ' ' | '\'' | '=' | ':' | '_' | '.' | '/')* quote

let address = '@' (letter | digit)*
let key_hash = 'h' string 
let key = 'k' string 
let signature = 's' string 
let bytes = 'b' string 
let chain_id = '0' 'x' hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit 

rule token = parse 
  | newline         { Lexing.new_line lexbuf; token lexbuf }
  | blank+          { token lexbuf }
  | int as i 			  { INT (int_of_string i) }
  | nat as i 			  { NAT (int_of_string (String.sub i 0 ((String.length i) - 1))) }
  | mtz as i 			  { MTZ (int_of_string (String.sub i 0 ((String.length i) - 3))) } 
  | tz as i 			  { MTZ (int_of_float (1000000. *. float_of_string (String.sub i 0 ((String.length i) - 2)))) } 

  | "interface"     { INTERFACE }
  | "contract"      { CONTRACT }
  | "entry"         { ENTRY }
	| "view"					{ VIEW }
  | "extends"       { EXTENDS }
  | "implements"    { IMPLEMENTS }
  | "import"        { IMPORT }
  | "function"      { FUNCTION }
  | "field"				  { FIELD }
  | "type"          { TYPE }
  | "enum"          { ENUM }
  | "list"          { CONT "list" }
  | "map"           { CONT "map" }
  | "big_map"       { CONT "big_map" }
  | "option"        { CONT "option" }
  | "set"           { CONT "set" }
  (* | "callback"      { CONT "callback" } *)
  | "const"				  { CONST }
  | "record"        { RECORD }
  | "this"				  { THIS }
  | "if"				  	{ IF }
  | "then"				  { THEN }
  | "else"				  { ELSE }
  | "and"				  	{ AND }
  | "or"				  	{ OR }
  | "not"				  	{ NOT }
  | "Some"				  { SOME }
  | "None"				  { NONE }
  | "Tezos"				  { TEZOS }
	| "match"					{ MATCH }
	| "with"					{ WITH }
	| "Unit"					{ UNIT }
	| "Crypto"				{ CRYPTO }
  | "constructor"		{ CONSTRUCTOR }
	| "let"						{ LET }
	| "in"						{ IN }
	| "pragma"				{ PRAGMA }
  
	| "#"							{ HT }
  | "->"				 	 	{ LAMBDA }
  | "=>"				  	{ LAMBDAB }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["					  	{ LSQUARE }
  | "]"					  	{ RSQUARE }
  | "."					  	{ DOT }
  | "("             { LPAR }
  (* | "@"					 		{ AT } *)
  | ")"             { RPAR }
  | ","             { COMMA }
  | ":"             { COLON }
  | ";"             { SEMICOLON }
	| "_"							{ UNDERSCORE }
  | "|"             { PIPE }
  (* | "\""				  	{ QUOTE } *)
  (* | "?"					  	{ QUESTION } *)

  | "+"					  	{ ADD }
  | "-"					  	{ SUB }
  | "/"					  	{ DIV }
  | "*"					  	{ MUL }
  | "%"					  	{ MOD }
  | "="             { EQ }
  | "=="            { EQEQ }
  | "!="				  	{ NEQ }
  | ">"					  	{ GT }
  | "<"					  	{ LT }
  | "<="			      { LTE }
  | ">="			      { GTE }
  | "true"				  { TRUE }
  | "false"				  { FALSE }

  | "//"            { comment_line lexbuf; token lexbuf }
  | "(*"            { comment_multiline lexbuf; token lexbuf }

  | address as s    { ADDRESS (String.sub s 1 ((String.length s) - 1)) }
	| chain_id as s		{ CHAIN_ID (int_of_string s) }
  | string as s     { STRING (String.sub s 1 ((String.length s) - 2)) }
  | bytes as s     	{ BYTES (String.sub s 2 ((String.length s) - 3)) }
  | key as s     		{ KEY (String.sub s 2 ((String.length s) - 3)) }
  | key_hash as s   { KEY_HASH (String.sub s 2 ((String.length s) - 3)) }
  | signature as s  { SIGNATURE (String.sub s 2 ((String.length s) - 3)) }

  | modifier as m   { MODIFIER m }
  | ident as i      { if List.exists (fun r -> r = i) reserved then raise (SyntaxError2 ("Using reserved word for identifier")) else IDENT i }

  | eof             { EOF }
  | _ as c          { raise (SyntaxError2 (Format.sprintf "Invalid string starting with %C" c)) }

and comment_line = parse
  | "//"      			{ comment_line lexbuf }
  | newline   			{ () }
  | _         			{ comment_line lexbuf }

and comment_multiline = parse
  | "*)"   					{ () }
  | eof    					{ failwith "unterminated comment" }
  | newline					{ new_line lexbuf; comment_multiline lexbuf }
  | _      					{ comment_multiline lexbuf }