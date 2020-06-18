open Printf
open Lexing

type empt = | Decl of Parse_tree.declaration | Iden of Parse_tree.iden | Expr of Parse_tree.pexpr

module LocationTable = Ephemeron.K1.Make(struct
  type t = empt
  let hash = Hashtbl.hash (* or your own hash if you have one *)
  let equal = (=) (* or a specilized equal operator *)
end) 

let filename = ref ""
let locations = ref @@ LocationTable.create 10000
let ladd = LocationTable.add !locations

(* p.pos_lnum, p.pos_cnum - p.pos_bol *)

let pp_pos s e = !filename, s.pos_lnum, (s.pos_cnum - s.pos_bol + 1)

type l = (string * int * int )

let locd s e x = ladd (Decl (x)) @@ pp_pos s e; x
let loce s e x = ladd (Expr (x)) @@ pp_pos s e; x
let loci s e x = ladd (Iden (x)) @@ pp_pos s e; x



let dline p = try Some (LocationTable.find !locations @@ Decl (List.hd p)) with | _ -> None
let eline ee = try Some (LocationTable.find !locations @@ Expr (ee)) with | _ -> None
let iline ii = try Some (LocationTable.find !locations @@ Iden (ii)) with | _ -> None 