type ttype = 
  | TAddress
  | TInt
  | TNat
  | TMutez
  | TTimestamp
  | TBool
  | TString
  | TBytes
  | TLambda of ttype * ttype
  | TEnum of string list
  | TList of ttype
  | TSet of ttype
  | TMap of ttype * ttype 
  | TBigMap of ttype * ttype
  | TRecord of (string * ttype) list
  | TTuple of ttype list 
  | TContract of ttype 
  | TCallback of ttype
  [@@deriving show {with_path = false}]


type expr = 
  | Empty
  | None
  | Bool of bool
  | Nat of int 
  | Int of int 
  | Mutez of int
  | Address of string
  | String of string
  | Bytes of bytes
  | Some of expr
  | Enum of string * string
  | Typed of expr * ttype
  | List of expr list 
  | Set of expr list 
  | Map of (expr * expr) list
  | BigMap of (expr * expr) list
  | Tuple of expr list
  | Lambda of (string * ttype) list * expr
  | Record of (string * expr) list

  (* aritmetic *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr

  (* bool *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Lte of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr

  | IfThenElse of expr * expr * expr 

  [@@deriving show {with_path = false}]

type statement = 
  | Skip
  [@@deriving show {with_path = false}]

type declaration =
  | None
  [@@deriving show {with_path = false}]

type t = {
  a: int;
}




let of_parse_tree (p: Parse_tree.t) = {a=0}