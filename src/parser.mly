%{
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, QUOTE, LSQUARE, RSQUARE, AT
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION, FIELD
%token ENUM, TYPE, RECORD, CONST
%token <string> MODIFIER
%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <string> CONT

%start <Parse_tree.t> program

%%
	program: il=list(dimport) dl=list(declaration) EOF { il @ dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  value: 
    | x=STRING { Parse_tree.PVString (x) } (* a string, a key, a key_hash, an address*)
    | x=INT { Parse_tree.PVInt (x) } 
    | x=IDENT DOT y=IDENT { Parse_tree.PVEnum (x, y) } (* enum value *)
    | x=IDENT { Parse_tree.PVRef (x) }
    | LPAR vl=separated_list(COMMA, value) RPAR { Parse_tree.PVTuple (vl) }
    | LSQUARE vl=separated_list(COMMA, value) RSQUARE { Parse_tree.PVList (vl) }
  
  tvalue:
    | v=value { v }
    | v=value COLON t=type_sig { v }

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

  dfunction:
    | FUNCTION n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR COLON tt=type_sig LBRACE RBRACE
      { Parse_tree.DFunction (n, pl, tt, ()) }

	dinterface: 
    | INTERFACE x=IDENT LBRACE sl=list(terminated(signature, SEMICOLON)) RBRACE   
      { Parse_tree.DInterface (x, None, sl) }
    | INTERFACE x=IDENT EXTENDS e=IDENT LBRACE sl=list(terminated(signature, SEMICOLON)) RBRACE  
      { Parse_tree.DInterface (x, Some(e), sl) }

  dcontract_field:
    | FIELD x=IDENT COLON t=type_sig EQ v=tvalue
      { (x, t, v) }

  dcontract_entry:
    | ENTRY x=IDENT LPAR tl=separated_list(COMMA, parameter) RPAR LBRACE RBRACE
      { (x, tl, ()) }

  dcontract_body:
    | fl=list(terminated(dcontract_field, SEMICOLON)) el=list(dcontract_entry)
      { (fl, el) }

  dcontract:
    | CONTRACT x=IDENT EXTENDS e=IDENT IMPLEMENTS i=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract (x, Some(e), Some(i), fst b, snd b)}
    | CONTRACT x=IDENT IMPLEMENTS i=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract (x, None, Some(i), fst b, snd b)}
    | CONTRACT x=IDENT EXTENDS e=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract (x, Some(e), None, fst b, snd b)}
    | CONTRACT x=IDENT LBRACE b=dcontract_body RBRACE
      { Parse_tree.DContract (x, None, None, fst b, snd b)}

  dtype:
    | TYPE x=IDENT EQ tl=type_sig SEMICOLON
      { Parse_tree.DType (x, tl) }

  dconst:
    | CONST x=IDENT COLON t=type_expr EQ v=tvalue SEMICOLON
      { Parse_tree.DConst (x, t, v) }

  declaration:
    | i=dinterface { i }
    | c=dcontract  { c }
    | f=dfunction  { f }
    | t=dtype      { t }
    | cc=dconst    { cc }
    // | m=modifier  { m }
