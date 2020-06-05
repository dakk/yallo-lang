

let main () = 
  Compiler.compile Sys.argv.(1) Compiler.default_options |> ignore;
  Printf.printf "compiled!\n%!"

let () = main ()