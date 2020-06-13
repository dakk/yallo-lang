open Ast_env
open Translate_pdecl

type t = Env.t

let of_parse_tree (p: Parse_tree.t): t = 
  transform p Env.start_env 
  