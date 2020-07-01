%token EOF, LBRACE, RBRACE, LPAR, RPAR
%token STORAGE, PARAMETER, CODE
%token <string> ANNOT
%token <string> STRING


%start <Ast.t> program

%%
  program: 
		| PARAMETER pt=type_sig STORAGE st=type_sig CODE LBRACE RBRACE EOF
			{ 
				Ast.{
					parameter=pt;
					storage=st;
					code=();
				}
			}

	type_sig:
		| x=STRING 
			{ Ast.Iden (x) }
		| LPAR t=type_sig RPAR
			{ t }
		| s1=STRING x=ANNOT s2=type_sig s3=type_sig 
			{ Ast.Annotated (x, Ast.Comb(s1, s2, s3)) }
		| s1=STRING s2=type_sig s3=type_sig 
			{ Ast.Comb(s1, s2, s3) }
