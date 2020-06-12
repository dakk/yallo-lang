open Ast_ttype
open Ast_expr
open Ast_statement
open Ast_env
open Ast_decl
open Translate_ptype
open Translate_pexpr
open Translate_pdecl


(* in teoria dobbiamo estrarre tutto in un passaggio, altrimenti non siamo a conoscenza di cosa e' gia' definito o meno *) 
(* quando abbiamo un espressione, per esempio da assegnare ad una const, allora controlliamo se l'espressione
  ha lo stesso tipo della const *)
(* of_parse_tree returns the contract, with the env around it, so it needs also the contract to extract *)
(* creare un exception per ogni tipo di errore, catcharla nel compiler *)

let of_parse_tree (p: Parse_tree.t) = 
  transform p Env.start_env 
  