%{
  (* (a,b,c) destructing *)
  let t3trd (a,b,c) = c
  let t3fst (a,b,c) = a
  let t3snd (a,b,c) = b
%}

%token EOF
// QUOTE SIZE HT ASTERISK AT GET HAS QUESTION ASSERT
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, LSQUARE, RSQUARE
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, IMPORT, FUNCTION, FIELD
%token ENUM, TYPE, RECORD, CONST, THIS, AND, OR, NOT, LAMBDA, TRUE, FALSE
%token ADD, SUB, DIV, MUL, MOD, IF, THEN, ELSE, WITH, MATCH
%token LTE, LT, GT, GTE, EQEQ, NONE, SOME, HT, LET, IN
%token TEZOS, CONSTRUCTOR, LAMBDAB, NEQ, UNIT, CRYPTO
%token <string> MODIFIER
%token <string> IDENT
%token <string> STRING
%token <string> ADDRESS
%token <string> BYTES
%token <int> CHAIN_ID
%token <string> SIGNATURE
%token <string> KEY
%token <string> KEY_HASH
%token <int> INT
%token <int> NAT
%token <int> MTZ
%token <string> CONT

%left EQEQ, NEQ
%left LTE, LT, GT, GTE
%left ADD, SUB
%left MOD
%left MUL, DIV

%start <Parse_tree.t> program

%%
  program: il=list(dimport) dl=list(declaration) EOF { il @ dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  ident: | i=IDENT { i }

  signature: 
		// ml=list(MODIFIER)
    | ENTRY n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR 
      { (n, pl) }

  type_sig:
    | t=ident                                       { Parse_tree.PTBuiltin (t) }
    | LPAR t1=type_sig COMMA tl=separated_nonempty_list(COMMA, type_sig) RPAR         
                                                    { Parse_tree.PTTuple (t1::tl) }
    | p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }
    | bt=type_expr c=CONT                           { Parse_tree.PTCont (c, bt) }
    | bt=type_expr CONTRACT                         { Parse_tree.PTCont ("contract", bt) }
    | RECORD LBRACE tl=separated_nonempty_list(COMMA, parameter) RBRACE
                                                    { Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR { Parse_tree.PTEnum (el) }
    | LPAR t=type_sig RPAR											    { t }

  type_expr: | te=type_sig {te}

  dimport: | IMPORT p=STRING SEMICOLON { Parse_tree.DImport (p)}

  emap_element:
    | LPAR a=expr COLON b=expr RPAR { (a, b) }
  erec_element:
    | i=IDENT EQ b=expr { (i, b) }
	match_case:
		| PIPE e=expr LAMBDA v=expr { (e, v) }


	left:
		| l=left DOT i=IDENT				{ Parse_tree.PEDot (l, i) }
    | i=IDENT 						      { Parse_tree.PERef (i) }
    | TEZOS DOT i=IDENT         { Parse_tree.PETRef (i) }
    | THIS DOT i=IDENT          { Parse_tree.PESRef (i) }
    | CRYPTO DOT i=IDENT 				{ Parse_tree.PECRef (i) }

  expr:
		| UNIT						{ Parse_tree.PEUnit }
    | NONE  					{ Parse_tree.PENone }
    | TRUE            { Parse_tree.PEBool (true) }
    | FALSE           { Parse_tree.PEBool (false) }
		| x=CHAIN_ID			{ Parse_tree.PEChainId (x) }
    | x=STRING 				{ Parse_tree.PEString (x) }
    | x=SIGNATURE 		{ Parse_tree.PESignature (x) }
    | x=BYTES			 		{ Parse_tree.PEBytes (x) }
    | x=KEY				 		{ Parse_tree.PEKey (x) }
    | x=KEY_HASH	 		{ Parse_tree.PEKeyHash (x) }
    | x=ADDRESS		 		{ Parse_tree.PEAddress (x) }
    | x=MTZ 					{ Parse_tree.PEMutez (x) }
    | x=NAT 					{ Parse_tree.PENat (x) }
    | x=INT 					{ Parse_tree.PEInt (x) }
    | SOME LPAR x=expr RPAR 	  { Parse_tree.PESome (x) }
    | LBRACE tl=separated_nonempty_list(COMMA, erec_element) RBRACE
                                { Parse_tree.PERecord (tl) }
    | LSQUARE tl=separated_list(COMMA, expr) RSQUARE
                                { Parse_tree.PEList (tl) }
    | LPAR t=expr COMMA tl=separated_nonempty_list(COMMA, expr) RPAR
                                { Parse_tree.PETuple (t::tl) }
    | LSQUARE tl=separated_nonempty_list(COMMA, emap_element) RSQUARE
                                { Parse_tree.PEMap (tl) }
    | LPAR tl=separated_nonempty_list(COMMA, parameter) RPAR LAMBDAB LPAR e=expr RPAR
                                { Parse_tree.PELambda (tl, e) }

		// bindings 
		| LET i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { Parse_tree.PELetIn (i, Some(t), e, ee) }
		| LET i=IDENT COLON t=type_sig EQ e=expr { Parse_tree.PELet (i, Some(t), e) }
		| LET i=IDENT EQ e=expr IN ee=expr { Parse_tree.PELetIn (i, None, e, ee) }
		| LET i=IDENT EQ e=expr { Parse_tree.PELet (i, None, e) }

		| LET LPAR tl=separated_nonempty_list(COMMA, parameter) RPAR EQ e=expr IN ee=expr 
			{ Parse_tree.PELetTupleIn (tl, e, ee) }
		| LET LPAR tl=separated_nonempty_list(COMMA, parameter) RPAR EQ e=expr 
			{ Parse_tree.PELetTuple (tl, e) }

		// storage assignment
		| i=left EQ e=expr { Parse_tree.PEAssign (i, e) }

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

		// match with
		| MATCH c=expr WITH cl=nonempty_list(match_case) 
																{ Parse_tree.PEMatchWith (c, cl) }

    | i=IDENT 						      { Parse_tree.PERef (i) }
    | e=left DOT i=IDENT 				{ Parse_tree.PEDot (e, i) }
    | e=expr DOT i=IDENT 				{ Parse_tree.PEDot (e, i) }
		| THIS DOT i=IDENT					{ Parse_tree.PESRef (i) }
    | ii=IDENT HT i=IDENT 			{ Parse_tree.PEHt (ii, i) }

    // apply a function
    | i=left LPAR p=separated_list(COMMA, expr) RPAR 						      { PEApply(i, p) }
    | i=expr LPAR p=separated_list(COMMA, expr) RPAR 						      { PEApply(i, p) }
		
    | LPAR e=expr RPAR 				  { e }
    | LPAR v=expr COLON t=type_sig RPAR { Parse_tree.PETyped (v, t) }

  fun_body:
		| e1=expr SEMICOLON f=fun_body { Parse_tree.PESeq (e1, f) }
		| e1=expr { e1 }

  dfunction:
    | FUNCTION n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR COLON tt=type_sig LBRACE b=fun_body RBRACE
      { Parse_tree.DFunction ({id=n; params=pl; rettype=tt; exp=b }) }

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
      { Parse_tree.DConst ({ id=x; t=Some(t); v=v }) }
    | CONST x=IDENT EQ v=expr SEMICOLON
      { Parse_tree.DConst ({ id=x; t=None; v=v }) }

  declaration:
    | i=dinterface { i }
    | c=dcontract  { c }
    | f=dfunction  { f }
    | t=dtype      { t }
    | cc=dconst    { cc }
    // | m=modifier  { m }


  // braced (S):
  // | LPAR s=S RPAR { s }
