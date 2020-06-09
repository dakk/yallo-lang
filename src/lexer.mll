{
  open Parser
  open Lexing

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
	"true";
	"false";
	"bool";
	"and";
	"or";
	"not";
	"skip";
	"size";
	"assert";
	"constructor";
  ]
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

let blank = [' ' '\t' '\r']
let newline = '\n'
let quote = '"'
let string = quote (letter | '_' | '.' | '/')* quote

rule token = parse 
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }
  | int as i 			  { INT (int_of_string i) }
  | nat as i 			  { NAT (int_of_string (String.sub i 0 ((String.length i) - 1))) }
  | mtz as i 			  { MTZ (int_of_string (String.sub i 0 ((String.length i) - 3))) } 

  | "interface"           { INTERFACE }
  | "contract"            { CONTRACT }
  | "entry"               { ENTRY }
  | "extends"             { EXTENDS }
  | "implements"          { IMPLEMENTS }
  | "import"              { IMPORT }
  | "function"            { FUNCTION }
  | "field"				  { FIELD }
  | "type"                { TYPE }
  | "enum"                { ENUM }
  | "list"                { CONT "list" }
  | "map"                 { CONT "map" }
  | "big_map"             { CONT "big_map" }
  | "option"              { CONT "option" }
  | "set"                 { CONT "set" }
  | "callback"            { CONT "callback" }
  | "const"				  { CONST }
  | "record"              { RECORD }
  | "var"				  { VAR }
  | "return"			  { RETURN }
  | "this"				  { THIS }
  | "if"				  { IF }
  | "then"				  { THEN }
  | "else"				  { ELSE }
  | "and"				  { AND }
  | "or"				  { OR }
  | "not"				  { NOT }
  | "skip"				  { SKIP }
  | "size"				  { SIZE }
  | "get"				  { GET }
  | "has"				  { HAS }
  | "empty"				  { EMPTY }
  | "assert"			  { ASSERT }
  | "Some"				  { SOME }
  | "None"				  { NONE }
  | "Tezos"				  { TEZOS }
  | "constructor"		  { CONSTRUCTOR }
  
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "["					  { LSQUARE }
  | "]"					  { RSQUARE }
  | "."					  { DOT }
  | "("                   { LPAR }
  | "@"					  { AT }
  | ")"                   { RPAR }
  | ","                   { COMMA }
  | ":"                   { COLON }
  | ";"                   { SEMICOLON }
  | "|"                   { PIPE }
  | "\""				  { QUOTE }
  | "?"					  { QUESTION }

  | "+"					  { ADD }
  | "-"					  { SUB }
  | "/"					  { DIV }
  | "*"					  { MUL }
  | "%"					  { MOD }
  | "="                   { EQ }
  | "=="                  { EQEQ }
  | ">"					  { GT }
  | "<"					  { LT }
  | "<="			      { LTE }
  | ">="			      { GTE }
  | "true"				  { TRUE }
  | "false"				  { FALSE }

  | "//"                  { comment_line lexbuf; token lexbuf }

  | string as s           { STRING (String.sub s 1 ((String.length s) - 2)) }
  | modifier as m         { MODIFIER m }
  | ident as i            { if List.exists (fun r -> r = i) reserved then failwith "Using reserved word for identifier %s" i else IDENT i }

  | eof                   { EOF }
  | _ as c                { failwith (Format.sprintf "invalid string starting with %C" c) }

and comment_line = parse
  | "//"      { comment_line lexbuf }
  | newline   { () }
  | _         { comment_line lexbuf }
