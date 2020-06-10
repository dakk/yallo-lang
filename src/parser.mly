%{
  (* (a,b,c) destructing *)
  let t3trd (a,b,c) = c
  let t3fst (a,b,c) = a
  let t3snd (a,b,c) = b
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, QUOTE, LSQUARE, RSQUARE, AT
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION, FIELD, VAR
%token ENUM, TYPE, RECORD, CONST, RETURN, THIS, AND, OR, NOT, LAMBDA, TRUE, FALSE
%token ADD, SUB, DIV, MUL, MOD, IF, THEN, ELSE, SKIP
%token LTE, LT, GT, GTE, EQEQ, SIZE, QUESTION, GET, HAS, EMPTY, NONE, SOME
%token TEZOS, ASSERT, CONSTRUCTOR, ASTERISK, LAMBDAB, NEQ, HT
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

  ident: | i=IDENT { i }

  signature: 
    | ENTRY n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR ml=list(MODIFIER)
      { (n, pl, []) }

  type_sig:
    | t=ident                                       { Parse_tree.PTBuiltin (t) }
    | LPAR t1=type_sig COMMA tl=separated_nonempty_list(COMMA, type_sig) RPAR         
                                                    { Parse_tree.PTTuple (t1::tl) }
    | p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }
    | bt=type_expr c=CONT                           { Parse_tree.PTCont (c, bt) }
    | bt=type_expr CONTRACT                         { Parse_tree.PTCont ("contract", bt) }
    | RECORD LBRACE tl=separated_nonempty_list(SEMICOLON, parameter) RBRACE
                                                    { Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR { Parse_tree.PTEnum (el) }
    | LPAR t=type_sig RPAR											    { t }

  type_expr: | te=type_sig {te}

  dimport: | IMPORT p=STRING SEMICOLON { Parse_tree.DImport (p)}

  emap_element:
    | LPAR a=expr COLON b=expr RPAR { (a, b) }
  erec_element:
    | i=IDENT EQ b=expr { (i, b) }

  expr:
    | NONE  					{ Parse_tree.PENone }
    | EMPTY 					{ Parse_tree.PEEmpty }
    | TRUE            { Parse_tree.PEBool (true) }
    | FALSE           { Parse_tree.PEBool (false) }
    | x=STRING 				{ Parse_tree.PEString (x) }
    | x=MTZ 					{ Parse_tree.PEMutez (x) }
    | x=NAT 					{ Parse_tree.PENat (x) }
    | x=INT 					{ Parse_tree.PEInt (x) }
    | SOME LPAR x=expr RPAR 	  { Parse_tree.PESome (x) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, erec_element) RBRACE
                                { Parse_tree.PERecord (tl) }
    | LSQUARE tl=separated_list(COMMA, expr) RSQUARE
                                { Parse_tree.PEList (tl) }
    | LPAR t=expr COMMA tl=separated_nonempty_list(COMMA, expr) RPAR
                                { Parse_tree.PETuple (t::tl) }
    | LSQUARE tl=separated_nonempty_list(COMMA, emap_element) RSQUARE
                                { Parse_tree.PEMap (tl) }
    | LPAR tl=separated_nonempty_list(COMMA, parameter) RPAR LAMBDAB LPAR e=expr RPAR
                                { Parse_tree.PELambda (tl, e) }

    // arithm
    | e1=expr ADD e2=expr 			{ Parse_tree.PEAdd (e1,e2) }
    | e1=expr SUB e2=expr 			{ Parse_tree.PESub (e1,e2) }
    | e1=expr DIV e2=expr 			{ Parse_tree.PEDiv (e1,e2) }
    | e1=expr MUL e2=expr 			{ Parse_tree.PEMul (e1,e2) }
    | e1=expr MOD e2=expr 			{ Parse_tree.PEMod (e1,e2) }

    // boolean
    | e1=expr AND e2=expr 			{ Parse_tree.PEAnd (e1,e2) }
    | e1=expr OR e2=expr 			  { Parse_tree.PEOr (e1,e2) }
    | NOT e=expr 					      { Parse_tree.PENot (e) }
    | e1=expr LT e2=expr 			  { Parse_tree.PELt (e1,e2) }
    | e1=expr LTE e2=expr 			{ Parse_tree.PELte (e1,e2) }
    | e1=expr GT e2=expr 			  { Parse_tree.PEGt (e1,e2) }
    | e1=expr GTE e2=expr 			{ Parse_tree.PEGte (e1,e2) }
    | e1=expr EQEQ e2=expr 			{ Parse_tree.PEEq (e1,e2) }
    | e1=expr NEQ e2=expr 			{ Parse_tree.PENeq (e1,e2) }

    // if then else
    | IF c=expr THEN e1=expr ELSE e2=expr 
                                { Parse_tree.PEIfThenElse (c,e1,e2) }
    // apply a function
    | i=expr LPAR p=separated_list(COMMA, expr) RPAR 						      { PEApply(i, p) }
    // | f=expr LPAR p=separated_list(COMMA, expr) RPAR 
    //                             { Parse_tree.PEApply (f, p) }
    | LPAR e=expr RPAR 				  { e }
    | LPAR v=expr COLON t=type_sig RPAR { Parse_tree.PETyped (v, t) }


    // | i=IDENT LPAR p=separated_list(COMMA, expr) RPAR 						      { PEApply(Parse_tree.PERef (i), p) }
    // | TEZOS DOT i=IDENT LPAR p=separated_list(COMMA, expr) RPAR        { PEApply(Parse_tree.PETRef (i), p) }
    // | THIS DOT i=IDENT LPAR p=separated_list(COMMA, expr) RPAR         { PEApply(Parse_tree.PESRef (i), p) }
    | i=IDENT 						      { Parse_tree.PERef (i) }
    | TEZOS DOT i=IDENT         { Parse_tree.PETRef (i) }
    | THIS DOT i=IDENT          { Parse_tree.PESRef (i) }
    // todo: fix tezos.c() and this.c()
    // | THIS DOT ii=IDENT DOT i=IDENT LPAR p=separated_list(COMMA, expr) RPAR 			
    //   { PEApply2(Parse_tree.PESRef(ii), i, p) }
    | e=expr DOT i=IDENT { PEDot (e, i) }




  left_hand:
    | x=IDENT           { Parse_tree.I x }
    | THIS DOT x=IDENT  { Parse_tree.S x }
    | TEZOS DOT x=IDENT { Parse_tree.T x }

  statement:
    | VAR x=IDENT COLON t=type_expr SEMICOLON
      { Parse_tree.PSVar (x, t) }
    | VAR x=IDENT COLON t=type_expr EQ e=expr SEMICOLON
      { Parse_tree.PSVarAssign (x, t, e) }
    | VAR LPAR tl=separated_nonempty_list(COMMA, parameter) RPAR EQ e=expr SEMICOLON
      { Parse_tree.PSVarAssignTuple (tl, e) }
    | CONST x=IDENT COLON t=type_expr EQ e=expr SEMICOLON 
      { Parse_tree.PSConst (x, t, e) }
    | x=left_hand EQ e=expr SEMICOLON
      { Parse_tree.PSAssign (x, e) }
    | x=left_hand DOT i=ident EQ e=expr SEMICOLON
      { Parse_tree.PSRecAssign (x, i, e) }
    | i=IDENT LPAR p=separated_list(COMMA, expr) RPAR SEMICOLON
      { Parse_tree.PSCallBuiltin (i, p) }
    | x=left_hand DOT i=IDENT LPAR p=separated_list(COMMA, expr) RPAR SEMICOLON
      { Parse_tree.PSCall (x, i, p) }
    | RETURN x=expr SEMICOLON
      { Parse_tree.PSReturn x }
    | SKIP SEMICOLON 
      { Parse_tree.PSSkip }

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
    | THIS DOT x=IDENT EQ v=expr SEMICOLON
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
    | CONST x=IDENT COLON t=type_expr EQ v=expr SEMICOLON
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
