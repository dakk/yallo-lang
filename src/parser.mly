%{
%}

%token EOL, EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION
%token ENUM, TYPE, RECORD
%token <string> MODIFIER
%token <string> IDENT
%token <string> STRING
%token <string> CONT

%start <Parse_tree.t> program

%%
	program: il=list(dimport) dl=list(declaration) EOF { il @ dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

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

  dcontract:
    | CONTRACT x=IDENT EXTENDS e=IDENT IMPLEMENTS i=IDENT LBRACE RBRACE
      { Parse_tree.DContract (x, Some(e), Some(i), [])}
    | CONTRACT x=IDENT IMPLEMENTS i=IDENT LBRACE RBRACE
      { Parse_tree.DContract (x, None, Some(i), [])}
    | CONTRACT x=IDENT EXTENDS e=IDENT LBRACE RBRACE
      { Parse_tree.DContract (x, Some(e), None, [])}
    | CONTRACT x=IDENT LBRACE RBRACE
      { Parse_tree.DContract (x, None, None, [])}

  denum:
    | ENUM x=IDENT EQ el=separated_list(PIPE, ident) SEMICOLON
      { Parse_tree.DEnum (x, el) }

  dtype:
    | TYPE x=IDENT EQ tl=type_sig SEMICOLON
      { Parse_tree.DType (x, tl) }

  declaration:
    | i=dinterface { i }
    | c=dcontract  { c }
    | f=dfunction  { f }
    | t=dtype      { t }
    // | cc=const { m }
    // | m=modifier  { m }
