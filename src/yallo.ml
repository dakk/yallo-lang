open Core
open Yallo

let run action filename opt = 
  (match action with 
  | "compile" -> Compiler.compile filename opt
  | _ -> failwith @@ "Invalid compiler action: " ^ action
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
        } in run action filename opt)

let () = Command.run ~version:"0.1" ~build_info:"git" command