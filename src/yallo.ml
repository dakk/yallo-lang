open Core

let run action filename opt = 
  (match action with 
  | "compile" -> Compiler.compile filename opt
  );
  Printf.printf "done!\n%!"

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
      in fun () -> 
        let opt = Compiler.{
          contract = contract;
          print_pt = ppt;
          print_ast = past;
          verbose = verbose;
        } in run action filename opt)

let () = Command.run ~version:"0.1" ~build_info:"git" command