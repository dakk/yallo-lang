let main () = 
  Compiler.compile Sys.argv.(1) Sys.argv.(2) Sys.argv.(3) Compiler.default_options |> ignore;
  Printf.printf "compiled!\n%!"

let () = main ()