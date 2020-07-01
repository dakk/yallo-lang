type mtype = 
| Iden of string
| Comb of string * mtype * mtype
| Annotated of string * mtype

type t = {
  parameter: mtype;
  storage: mtype;
  code: unit;
}