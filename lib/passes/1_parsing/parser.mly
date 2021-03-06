%{
	open Parse_tree
	open Pt_loc 
%}

%token EOF
// QUOTE SIZE HT ASTERISK AT GET HAS QUESTION ASSERT
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, LSQUARE, RSQUARE
%token INTERFACE, CONTRACT, ENTRY, EXTENDS, IMPLEMENTS, FUNCTION, FIELD, VIEW
%token ENUM, TYPE, RECORD, CONST, THIS, AND, OR, NOT, TRUE, FALSE
%token ADD, SUB, DIV, MUL, MOD, IF, THEN, ELSE, WITH, MATCH
%token LTE, LT, GT, GTE, EQEQ, NONE, SOME, HT, LET, IN
%token TEZOS, CONSTRUCTOR, LAMBDAB, NEQ, UNIT, CRYPTO, UNDERSCORE
%token PRAGMA, IMPORT
%token LAMBDA
%token <string> MODIFIER
%token <string> STRING
%token <string> ADDRESS
%token <string> BYTES
%token <int> CHAIN_ID
%token <string> SIGNATURE
%token <string> KEY
%token <string> KEY_HASH
%token <Big_int.big_int> INT
%token <Big_int.big_int> NAT
%token <Big_int.big_int> MTZ
%token <string> CONT
%token <string> IDENT

%left NOT
%left OR
%left AND
%left EQEQ, NEQ
%left LTE, LT, GT, GTE
%left ADD, SUB
%left MOD
%left MUL, DIV

%start <Parse_tree.t> program

%%
  program: pl=list(dpragma) il=list(dimport) dl=list(declaration) EOF { il @ dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  param_opt_typed: 
	| i=IDENT COLON t=type_sig 	{ (i, Some(t)) }
	| i=IDENT  									{ (i, None) }

  ident: | i=IDENT { i }

  signature: 
		// ml=list(MODIFIER)
    | ENTRY n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR 
      { { id=n; arg=pl; ret=None } }
    | VIEW n=IDENT LPAR pl=separated_list(COMMA, parameter) RPAR COLON t=type_sig
      { { id=n; arg=pl; ret=Some(t) } }

  type_sig:
    | t=ident                                       { Parse_tree.PTBuiltin (t) }
    | bt=type_expr c=CONT                           { Parse_tree.PTCont (c, bt) }
    | LPAR t1=type_sig COMMA tl=separated_nonempty_list(COMMA, type_sig) RPAR         
                                                    { Parse_tree.PTTuple (t1::tl) }
    | bt=type_expr CONTRACT                         { Parse_tree.PTCont ("contract", bt) }
    | RECORD LBRACE tl=separated_nonempty_list(COMMA, parameter) RBRACE
                                                    { Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR { Parse_tree.PTEnum (el) }
    | LPAR t=type_sig RPAR											    { t }
    | p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }

  type_expr: | te=type_sig {te}

	dpragma: | PRAGMA p=STRING SEMICOLON { Parse_tree.DPragma (p) }
  dimport: | IMPORT p=STRING SEMICOLON { Parse_tree.DImport (p) }

  emap_element:
    | LBRACE a=expr COLON b=expr RBRACE { (a, b) }
  erec_element:
    | i=IDENT EQ b=expr { (i, b) }
	match_case:
		| PIPE e=expr LAMBDA v=expr { (e, v) }
		| PIPE UNDERSCORE LAMBDA v=expr { (loce $startpos $endpos @@ Parse_tree.PECaseDefault, v) }


	left:
		| l=left DOT i=IDENT				{ loce $startpos $endpos @@ Parse_tree.PEDot (l, i) }
    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }
    | TEZOS DOT i=IDENT         { loce $startpos $endpos @@ Parse_tree.PETRef (i) }
    | TEZOS DOT CONTRACT        { loce $startpos $endpos @@ Parse_tree.PETRef ("contract") }
    | THIS DOT i=IDENT          { loce $startpos $endpos @@ Parse_tree.PESRef (i) }
    | CRYPTO DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PECRef (i) }

  expr:
		| UNIT						{ loce $startpos $endpos @@ Parse_tree.PEUnit }
    | NONE  					{ loce $startpos $endpos @@ Parse_tree.PENone }
    | TRUE            { loce $startpos $endpos @@ Parse_tree.PEBool (true) }
    | FALSE           { loce $startpos $endpos @@ Parse_tree.PEBool (false) }
		| x=CHAIN_ID			{ loce $startpos $endpos @@ Parse_tree.PEChainId (x) }
    | x=STRING 				{ loce $startpos $endpos @@ Parse_tree.PEString (x) }
    | x=SIGNATURE 		{ loce $startpos $endpos @@ Parse_tree.PESignature (x) }
    | x=BYTES			 		{ loce $startpos $endpos @@ Parse_tree.PEBytes (x) }
    | x=KEY				 		{ loce $startpos $endpos @@ Parse_tree.PEKey (x) }
    | x=KEY_HASH	 		{ loce $startpos $endpos @@ Parse_tree.PEKeyHash (x) }
    | x=ADDRESS		 		{ loce $startpos $endpos @@ Parse_tree.PEAddress (x) }
    | x=INT 					{ loce $startpos $endpos @@ Parse_tree.PEInt (x) }
    | x=MTZ 					{ loce $startpos $endpos @@ Parse_tree.PEMutez (x) }
    | x=NAT 					{ loce $startpos $endpos @@ Parse_tree.PENat (x) }
    | SOME LPAR x=expr RPAR 	  { loce $startpos $endpos @@ Parse_tree.PESome (x) }
    | LBRACE tl=separated_nonempty_list(COMMA, erec_element) RBRACE
                                { loce $startpos $endpos @@ Parse_tree.PERecord (tl) }
    | LSQUARE tl=separated_list(COMMA, expr) RSQUARE
                                { loce $startpos $endpos @@ Parse_tree.PEList (tl) }
    | LPAR t=expr COMMA tl=separated_nonempty_list(COMMA, expr) RPAR
                                { loce $startpos $endpos @@ Parse_tree.PETuple (t::tl) }
    | LSQUARE tl=separated_nonempty_list(COMMA, emap_element) RSQUARE
                                { loce $startpos $endpos @@ Parse_tree.PEMap (tl) }
    | LPAR tl=separated_list(COMMA, parameter) RPAR LAMBDAB LPAR e=expr RPAR
                                { loce $startpos $endpos @@ Parse_tree.PELambda (tl, e) }

		// bindings 
		| LET i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, Some(t), e, ee) }
		| LET i=IDENT COLON t=type_sig EQ e=expr { loce $startpos $endpos @@ Parse_tree.PELet (i, Some(t), e) }
		| LET i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, None, e, ee) }
		| LET i=IDENT EQ e=expr { loce $startpos $endpos @@ Parse_tree.PELet (i, None, e) }

		| LET LPAR tl=separated_nonempty_list(COMMA, param_opt_typed) RPAR EQ e=expr IN ee=expr 
			{ loce $startpos $endpos @@ Parse_tree.PELetTupleIn (tl, e, ee) }
		| LET LPAR tl=separated_nonempty_list(COMMA, param_opt_typed) RPAR EQ e=expr 
			{ loce $startpos $endpos @@ Parse_tree.PELetTuple (tl, e) }

		// storage assignment
		| i=left EQ e=expr { loce $startpos $endpos @@ Parse_tree.PEAssign (i, e) }

    // arithm
    | e1=expr ADD e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEAdd (e1,e2) }
    | e1=expr SUB e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PESub (e1,e2) }
    | e1=expr DIV e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEDiv (e1,e2) }
    | e1=expr MUL e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEMul (e1,e2) }
    | e1=expr MOD e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEMod (e1,e2) }

    // boolean
    | e1=expr AND e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEAnd (e1,e2) }
    | e1=expr OR e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PEOr (e1,e2) }
    | NOT e=expr 					      { loce $startpos $endpos @@ Parse_tree.PENot (e) }
    | e1=expr LT e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PELt (e1,e2) }
    | e1=expr LTE e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PELte (e1,e2) }
    | e1=expr GT e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PEGt (e1,e2) }
    | e1=expr GTE e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEGte (e1,e2) }
    | e1=expr EQEQ e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEEq (e1,e2) }
    | e1=expr NEQ e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PENeq (e1,e2) }

    // if then else
    | IF c=expr THEN e1=expr ELSE e2=expr 
                                { loce $startpos $endpos @@ Parse_tree.PEIfThenElse (c,e1,e2) }

		// match with
		| MATCH c=expr WITH cl=nonempty_list(match_case) 
																{ loce $startpos $endpos @@ Parse_tree.PEMatchWith (c, cl) }

    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }
    | e=left DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }
    | e=expr DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }
		| THIS DOT i=IDENT					{ loce $startpos $endpos @@ Parse_tree.PESRef (i) }
    | ii=IDENT HT i=IDENT 			{ loce $startpos $endpos @@ Parse_tree.PEHt (ii, i) }

    // apply a function
    | i=left LPAR p=separated_list(COMMA, expr) RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
    | i=expr LPAR p=separated_list(COMMA, expr) RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
		
    | LPAR e=expr RPAR 				  { loce $startpos $endpos @@ e }
    | LPAR v=expr COLON t=type_sig RPAR { loce $startpos $endpos @@ Parse_tree.PETyped (v, t) }
		| LPAR e1=expr SEMICOLON f=fun_body RPAR { loce $startpos $endpos @@ Parse_tree.PESeq (e1, f) }

  fun_body:
		| e1=expr SEMICOLON f=fun_body { loce $startpos $endpos @@ Parse_tree.PESeq (e1, f) }
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
      { { id=x; arg=tl; pexpr=b; ret=None } }
    | VIEW x=IDENT LPAR tl=separated_list(COMMA, parameter) RPAR COLON t=type_sig LBRACE b=fun_body RBRACE
      { { id=x; arg=tl; pexpr=b; ret=Some(t) } }

  dcontract_constructor_assign:
    | THIS DOT x=IDENT EQ v=expr SEMICOLON
      { x, v }

  dcontract_constructor:
    | CONSTRUCTOR LPAR tl=separated_list(COMMA, parameter) RPAR LBRACE el=list(dcontract_constructor_assign) RBRACE
      { { arg=tl; pexprs=el } }

  dcontract_body:
    | fl=list(terminated(dcontract_field, SEMICOLON)) c=dcontract_constructor el=list(dcontract_entry)
      { (fl, el, Some(c)) }
    | fl=list(terminated(dcontract_field, SEMICOLON)) el=list(dcontract_entry)
      { (fl, el, None) }

  dcontract:
    | CONTRACT x=IDENT IMPLEMENTS i=IDENT LBRACE bb=dcontract_body RBRACE
      { 
				let (a,b,c) = bb in 
				Parse_tree.DContract (				
					{ id=x; implements=Some(i); fields=a; entries=b; constructor=c }
				) 
			}
    | CONTRACT x=IDENT LBRACE bb=dcontract_body RBRACE
      { 
				let (a,b,c) = bb in 
				Parse_tree.DContract (
					{ id=x; implements=None; fields=a; entries=b; constructor=c }
				) 
			}

  dtype:
    | TYPE x=IDENT EQ tl=type_sig SEMICOLON
      { Parse_tree.DType ({ id=x; t=tl }) }

  dconst:
    | CONST x=IDENT COLON t=type_expr EQ v=expr SEMICOLON
      { Parse_tree.DConst ({ id=x; t=Some(t); v=v }) }
    | CONST x=IDENT EQ v=expr SEMICOLON
      { Parse_tree.DConst ({ id=x; t=None; v=v }) }

  declaration:
    | i=dinterface { locd $startpos $endpos i }
    | c=dcontract  { locd $startpos $endpos c }
    | f=dfunction  { locd $startpos $endpos f }
    | t=dtype      { locd $startpos $endpos t }
    | cc=dconst    { locd $startpos $endpos cc }
    // | m=modifier  { m }


  // braced (S):
  // | LPAR s=S RPAR { s }
