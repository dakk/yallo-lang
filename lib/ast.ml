open Ast_env
open Translate_pdecl

let of_parse_tree (p: Parse_tree.t) = 
  transform p Env.start_env 
  