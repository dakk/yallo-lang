type pos = (string * int * int)

exception SyntaxError of string
exception ParsingError of string
exception TypeError of string
exception SymbolNotFound of string
exception DuplicateSymbolError of string
exception DeclarationError of string
exception InvalidExpression of string
exception ContractError of string
exception APIError of string
exception GenerateLigoError of string
exception CompilerError of string