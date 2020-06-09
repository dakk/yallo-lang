%{
  (* (a,b,c) destructing *)
  let t3trd (a,b,c) = c
  let t3fst (a,b,c) = a
  let t3snd (a,b,c) = b
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, QUOTE, LSQUARE, RSQUARE, AT
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION, FIELD, VAR
%token ENUM, TYPE, RECORD, CONST, RETURN, THIS, AND, OR, NOT
%token ADD, SUB, DIV, MUL, MOD, TRUE, FALSE, IF, THEN, ELSE, SKIP
%token LTE, LT, GT, GTE, EQEQ, SIZE, QUESTION, GET, HAS, EMPTY, NONE, SOME
%token TEZOS, ASSERT, CONSTRUCTOR
%token <string> MODIFIER
%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <int> NAT
%token <int> MTZ
%token <string> CONT

%start <Parse_tree.t> program

%%
	program: il=list(dimport) dl=list(declaration) EOF { il @ dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  value: 
    | NONE  { Parse_tree.PVNone }
    | EMPTY { Parse_tree.PVEmpty }
  
  tvalue:
    | v=value { v }
    | LPAR v=value COLON t=type_sig RPAR { Parse_tree.PVTyped (v, t) }

  ident: | i=IDENT { i }

  signature: 
    | ENTRY n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR ml=list(MODIFIER)
      { (n, pl, []) }

  type_sig:
    | t=ident                                                       { Parse_tree.PTBase (t) }
    | LPAR tl=separated_list(COMMA, type_sig) RPAR                  { Parse_tree.PTTuple (tl) }
    | bt=type_expr c=CONT                                           { Parse_tree.PTCont (c, bt) }
    | RECORD LBRACE tl=list(terminated(parameter, SEMICOLON)) RBRACE{ Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR                 { Parse_tree.PTEnum (el) }

  type_expr: | te=type_sig {te}

  dimport: | IMPORT p=STRING SEMICOLON { Parse_tree.DImport (p)}

  expr:
    | SEMICOLON { Parse_tree.PEVal (Parse_tree.None)}
  
  statement:
    | SEMICOLON { Parse_tree.PSSkip }

  fun_body:
    | sl=list(statement) { sl }

  dfunction:
    | FUNCTION n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR COLON tt=type_sig LBRACE b=fun_body RBRACE
      { Parse_tree.DFunction ({id=n; params=pl; rettype=tt; statements=b }) }

	dinterface: 
    | INTERFACE x=IDENT LBRACE sl=list(terminated(signature, SEMICOLON)) RBRACE   
      { Parse_tree.DInterface ({ id=x; extends=None; signatures=sl }) }
    | INTERFACE x=IDENT EXTENDS e=IDENT LBRACE sl=list(terminated(signature, SEMICOLON)) RBRACE  
      { Parse_tree.DInterface ({ id=x; extends=Some(e); signatures=sl }) }

  dcontract_field:
    | FIELD x=IDENT COLON t=type_sig
      { (x, t) }

  dcontract_entry:
    | ENTRY x=IDENT LPAR tl=separated_list(COMMA, parameter) RPAR LBRACE RBRACE
      { (x, tl, ()) }

  dcontract_constructor_assign:
    | THIS DOT x=IDENT EQ v=tvalue SEMICOLON
      { x, v }

  dcontract_constructor:
    | CONSTRUCTOR LPAR tl=separated_list(COMMA, parameter) RPAR LBRACE el=list(dcontract_constructor_assign) RBRACE
      { tl, el }

  dcontract_body:
    | fl=list(terminated(dcontract_field, SEMICOLON)) c=dcontract_constructor el=list(dcontract_entry)
      { (fl, el, Some(c)) }
    | fl=list(terminated(dcontract_field, SEMICOLON)) el=list(dcontract_entry)
      { (fl, el, None) }

  dcontract:
    | CONTRACT x=IDENT IMPLEMENTS i=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract ({ id=x; implements=Some(i); fields=t3fst b; entries=t3snd b; constructor=t3trd b }) }
    | CONTRACT x=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract ({ id=x; implements=None; fields=t3fst b; entries=t3snd b; constructor=t3trd b }) }

  dtype:
    | TYPE x=IDENT EQ tl=type_sig SEMICOLON
      { Parse_tree.DType ({ id=x; t=tl }) }

  dconst:
    | CONST x=IDENT COLON t=type_expr EQ v=tvalue SEMICOLON
      { Parse_tree.DConst ({ id=x; t=t; v=v }) }

  declaration:
    | i=dinterface { i }
    | c=dcontract  { c }
    | f=dfunction  { f }
    | t=dtype      { t }
    | cc=dconst    { cc }
    // | m=modifier  { m }


  // braced (S):
  // | LPAR s=S RPAR { s }
