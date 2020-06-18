open Core
open Yallo
open Errors

let run action filename opt = 
  (match action with 
  | "compile" -> Compiler.compile filename opt
  | "extract-interface" -> Compiler.extract_interface filename opt
  | _ -> raise @@ CompilerError ("Invalid compiler action: " ^ action)
  )

let command =
  Command.basic
    ~summary:"Yallo-lang compiler"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
      let open Command.Param in
      let%map
            action      = anon ("action" %: string)
        and filename  = anon ("filename" %: string)
        and contract  = flag "-contract" (optional string) ~doc:" selected contract"
        and past      = flag "-print-ast" no_arg ~doc:" print ast"
        and ppt       = flag "-print-pt" no_arg ~doc:" print parse-tree"
        and verbose   = flag "-verbose" no_arg ~doc:" enable verbosity"
        and out_lang  = flag "-out-lang" (optional string) ~doc:" output language"
      in fun () -> 
        let opt = Compiler.{
          out_lang = out_lang;
          contract = contract;
          print_pt = ppt;
          print_ast = past;
          verbose = verbose;
        } in (
          try 
            run action filename opt
          with 
          | CompilerError (m) -> print_endline @@ "CompilerError: " ^ m
          | SyntaxError (m) -> print_endline @@ "SyntaxError: " ^ m
          | ParsingError (m) -> print_endline @@ "ParsingError: " ^ m
          | TypeError (m) -> print_endline @@ "TypeError: " ^ m
          | SymbolNotFound (m) -> print_endline @@ "SymbolNotFound: " ^ m
          | DuplicateSymbolError (m) -> print_endline @@ "DuplicateSymbolError: " ^ m
          | DeclarationError (m) -> print_endline @@ "DeclarationError: " ^ m
          | InvalidExpression (m) -> print_endline @@ "InvalidExpression: " ^ m
          | ContractError (m) -> print_endline @@ "ContractError: " ^ m
          | APIError (m) -> print_endline @@ "APIError: " ^ m
          | GenerateLigoError (m) -> print_endline @@ "GenerateLigoError: " ^ m
        )
      )
        
let () = Command.run ~version:"0.1" ~build_info:"git" command