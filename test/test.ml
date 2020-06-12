open Yallo

let opt = Compiler.{
  contract = None;
  print_pt = false;
  print_ast = false;
  verbose = false;
}

let compile success path cname _ = 
  let compile_failure = try (Compiler.compile path { opt with contract=cname }; None) with 
  | f -> Some(f) in

  (match compile_failure, success with 
  | None, true -> ()
  | f, false -> ()
  | Some(f), _ -> failwith @@ "Invalid" ^ Printexc.to_string f)




let () =
  Alcotest.run "yallo" [
    "misc", [
      "declarations", `Quick, compile true "test/misc/declarations.yallo" None;
      "expr", `Quick, compile true "test/misc/expr.yallo" None;
      "literal", `Quick, compile true "test/misc/literal.yallo" None;
      "record", `Quick, compile true "test/misc/record.yallo" None;
      "statements", `Quick, compile true "test/misc/statements.yallo" None;
      "typemod", `Quick, compile true "test/misc/typemod.yallo" None;
      "types", `Quick, compile true "test/misc/types.yallo" None;
    ];
    "const", [
      "cont", `Quick, compile true "test/const/cont.yallo" None;
      "numeric", `Quick, compile true "test/const/numeric.yallo" None;
      "string", `Quick, compile true "test/const/string.yallo" None;
      "lambda", `Quick, compile true "test/const/lambda.yallo" None;
      "lambda_fail", `Quick, compile false "test/const/lambda_fail.yallo" None;
      "expr", `Quick, compile true "test/const/expr.yallo" None;
      "enum", `Quick, compile true "test/const/enum.yallo" None;
      "crypto_f", `Quick, compile true "test/const/crypto_f.yallo" None;
    ];
  ]