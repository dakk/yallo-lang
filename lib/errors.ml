exception SyntaxError of Loc.l option * string
exception ParsingError of Loc.l option * string
exception TypeError of Loc.l option * string
exception SymbolNotFound of Loc.l option * string
exception DuplicateSymbolError of Loc.l option * string
exception DeclarationError of Loc.l option * string
exception InvalidExpression of Loc.l option * string
exception ContractError of Loc.l option * string
exception APIError of Loc.l option * string
exception GenerateLigoError of Loc.l option * string
exception CompilerError of string
