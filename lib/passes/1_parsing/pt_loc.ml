open Printf
open Lexing

module PTEmp = struct 
  type t = | Decl of Parse_tree.declaration | Expr of Parse_tree.pexpr
  (* | Iden of Parse_tree.iden *)
end 

module LocationTable = Helpers.Loc.LocationTable (PTEmp)

type l = Helpers.Loc.l

let filename = ref ""
let locations = ref @@ LocationTable.create 10000
let ladd = LocationTable.add !locations
let pp_pos s e = s, !filename, s.pos_lnum, (s.pos_cnum - s.pos_bol + 1)

let locd s e x = ladd (Decl (x)) @@ pp_pos s e; x
let loce s e x = ladd (Expr (x)) @@ pp_pos s e; x
(* let loci s e x = ladd (Iden (x)) @@ pp_pos s e; x *)

let dline p = try Some (LocationTable.find !locations @@ Decl (List.hd p)) with | _ -> None
let eline ee = try Some (LocationTable.find !locations @@ Expr (ee)) with | _ -> None
(* let iline ii = try Some (LocationTable.find !locations @@ Iden (ii)) with | _ -> None  *)