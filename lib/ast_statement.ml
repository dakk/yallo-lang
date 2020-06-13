open Ast_ttype
open Ast_expr

type left_op = 
  | I of iden     (* i *)
  | S of iden     (* this.i *)
  [@@deriving show {with_path = false}]

type statement = 
  | FailIf of expr
  | FailIfMessage of expr * expr
  | Assert of expr    

  | MapUpdate of left_op * expr * expr 
  | BigMapUpdate of left_op * expr * expr 
  | SetUpdate of left_op * expr * expr 
  | MapRemove of left_op * expr 
  | BigMapRemove of left_op * expr 
  | ListIter of expr * expr
  | MapIter of expr * expr

  | DeclareConst of iden * ttype * expr
  | DeclareVar of iden * ttype
  | AssignVar of left_op * expr
  | RecAssign of left_op * iden * expr
  | DeclareAssignVar of iden * ttype * expr
  | DeclareAssignVarTuple of (iden *  ttype) list * expr
  
  | If of expr * statement list * (statement list) option          
  | Skip
  | Return of texpr