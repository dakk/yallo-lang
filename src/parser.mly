%{
  (* (a,b,c) destructing *)
  let t3trd (a,b,c) = c
  let t3fst (a,b,c) = a
  let t3snd (a,b,c) = b
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, QUOTE, LSQUARE, RSQUARE, AT
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION, FIELD, VAR
%token ENUM, TYPE, RECORD, CONST, RETURN, THIS, AND, OR, NOT, LAMBDA
%token ADD, SUB, DIV, MUL, MOD, TRUE, FALSE, IF, THEN, ELSE, SKIP
%token LTE, LT, GT, GTE, EQEQ, SIZE, QUESTION, GET, HAS, EMPTY, NONE, SOME
%token TEZOS, ASSERT, CONSTRUCTOR, ASTERISK, LAMBDAB
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


  vmap_element:
	| LPAR a=tvalue COLON b=tvalue RPAR { (a, b) }
  vrec_element:
	| i=IDENT EQ b=expr { (i, b) }

  value: 
    | NONE  					{ Parse_tree.PVNone }
	| SOME LPAR x=value RPAR 	{ Parse_tree.PVSome (x) }
    | EMPTY 					{ Parse_tree.PVEmpty }
	| x=STRING 					{ Parse_tree.PVString (x) }
	| x=MTZ 					{ Parse_tree.PVMutez (x) }
	| x=NAT 					{ Parse_tree.PVNat (x) }
	| x=INT 					{ Parse_tree.PVInt (x) }
	| LBRACE tl=separated_nonempty_list(SEMICOLON, vrec_element) RBRACE
								{ Parse_tree.PVRecord (tl) }
	| LSQUARE tl=separated_nonempty_list(COMMA, value) RSQUARE
								{ Parse_tree.PVList (tl) }
	| LPAR tl=separated_nonempty_list(COMMA, value) RPAR
								{ Parse_tree.PVTuple (tl) }
	| LSQUARE tl=separated_nonempty_list(COMMA, vmap_element) RSQUARE
								{ Parse_tree.PVMap (tl) }
	| LPAR tl=separated_nonempty_list(SEMICOLON, parameter) RPAR LAMBDAB LPAR e=expr RPAR
								{ Parse_tree.PVLambda (tl, e) }
  
  tvalue:
    | v=value { v }
    | LPAR v=value COLON t=type_sig RPAR { Parse_tree.PVTyped (v, t) }

  ident: | i=IDENT { i }

  signature: 
    | ENTRY n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR ml=list(MODIFIER)
      { (n, pl, []) }

  type_sig:
    | t=ident                                                       { Parse_tree.PTBase (t) }
    | LPAR t1=type_sig COMMA tl=separated_nonempty_list(COMMA, type_sig) RPAR         
																	{ Parse_tree.PTTuple (t1::tl) }
	| p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }
    | bt=type_expr c=CONT                                           { Parse_tree.PTCont (c, bt) }
    | bt=type_expr CONTRACT                                         { Parse_tree.PTCont ("contract", bt) }
    | RECORD LBRACE tl=separated_nonempty_list(SEMICOLON, parameter) RBRACE
																	{ Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR                 { Parse_tree.PTEnum (el) }
	| LPAR t=type_sig RPAR											{ t }

  type_expr: | te=type_sig {te}

  dimport: | IMPORT p=STRING SEMICOLON { Parse_tree.DImport (p)}

  expr:
    | t=tvalue { Parse_tree.PELit (t)}
  
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
    | ENTRY x=IDENT LPAR tl=separated_list(COMMA, parameter) RPAR LBRACE b=fun_body RBRACE
      { (x, tl, b) }

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
