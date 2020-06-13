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
      (* "declarations", `Quick, compile true "test/misc/declarations.yallo" None; *)
      "expr", `Quick, compile true "test/misc/expr.yallo" None;
      "literal", `Quick, compile true "test/misc/literal.yallo" None;
      "record", `Quick, compile true "test/misc/record.yallo" None;
      (* "statements", `Quick, compile true "test/misc/statements.yallo" None; *)
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
      "let_expr", `Quick, compile true "test/const/let_expr.yallo" None;
      "enum", `Quick, compile true "test/const/enum.yallo" None;
      "crypto_f", `Quick, compile true "test/const/crypto_f.yallo" None;
      "infer", `Quick, compile true "test/const/infer.yallo" None;
      "let_infer", `Quick, compile true "test/const/let_infer.yallo" None;
    ];
    "import", [
      "interface", `Quick, compile true "test/import/interface.yallo" None;
      "type", `Quick, compile true "test/import/type.yallo" None;
    ];
    "interface", [
      "i1", `Quick, compile true "test/interface/i1.yallo" None;
      "extend", `Quick, compile true "test/interface/extend.yallo" None;
      "empty", `Quick, compile true "test/interface/empty.yallo" None;
      "duplicate_fail", `Quick, compile false "test/interface/duplicate_fail.yallo" None;
      "dup_entry_fail", `Quick, compile false "test/interface/dup_entry_fail.yallo" None;
    ];
    "function", [
      "wrong_return_type", `Quick, compile false "test/function/wrong_return_type.yallo" None;
      "base_fun", `Quick, compile true "test/function/base_fun.yallo" None;
    ];
    "contract", [
      "token", `Quick, compile true "test/contract/token.yallo" None;
    ]
  ]