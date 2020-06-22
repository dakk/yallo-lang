open Core
open Yallo
open Helpers.Errors

let run action filename opt = 
  (match action with 
  | "compile" -> Compiler.compile filename opt
  (* | "build-storage" ->  *)
  (* | "build-parameter" ->  *)
  | "extract-interface" -> Compiler.extract_interface filename opt
  | _ -> raise @@ CompilerError ("Invalid compiler action: " ^ action)
  )

let summary = ""
^ "=== actions ===\n\n"
^ "  compile file.yallo [-dcontract ContractName] [-target ligo]\n"
^ "                 compiles a contract ContractName to ligo language\n\n"
^ "  extract-interface file.yallo -dcontract ContractName\n"
^ "                 extracts the yallo interface for the given contract\n"


let command =
  Command.basic
    ~summary:"Yallo-lang compiler"
    ~readme:(fun () -> summary)
    (let open Command.Let_syntax in
      let open Command.Param in
      let%map
            action      = anon ("action" %: string)
        and filename  = anon ("filename" %: string)
        and contract  = flag "-contract" (optional string) ~doc:" selected contract"
        and past      = flag "-print-ast" no_arg ~doc:" print ast"
        and ppt       = flag "-print-pt" no_arg ~doc:" print parse-tree"
        and verbose   = flag "-verbose" no_arg ~doc:" enable verbosity"
        and target  = flag "-target" (optional string) ~doc:" target language"
      in fun () -> 
        let opt = Compiler.{
          target = if is_none target then Some("ligo") else target;
          contract = contract;
          print_pt = ppt;
          print_ast = past;
          verbose = verbose;
        } in (
          let rec pp_err p cc m = pp_message p cc m true in 
          try 
            run action filename opt
          with 
          | CompilerError (m) -> print_endline @@ pp_err None "CompilerError" m
          | SyntaxError (p,m) -> print_endline @@ pp_err p "SyntaxError" m
          | ParsingError (p,m) -> print_endline @@ pp_err p "ParsingError" m
          | TypeError (p,m) -> print_endline @@ pp_err p "TypeError" m
          | SymbolNotFound (p,m) -> print_endline @@ pp_err p "SymbolNotFound" m
          | DuplicateSymbolError (p,m) -> print_endline @@ pp_err p "DuplicateSymbolError" m
          | DeclarationError (p,m) -> print_endline @@ pp_err p "DeclarationError" m
          | InvalidExpression (p,m) -> print_endline @@ pp_err p "InvalidExpression" m
          | ContractError (p,m) -> print_endline @@ pp_err p "ContractError" m
          | APIError (p,m) -> print_endline @@ pp_err p "APIError" m
          | GenerateLigoError (p,m) -> print_endline @@ pp_err p "GenerateLigoError" m
        )
      )
        
let () = Command.run ~version:"0.1" ~build_info:"git" command