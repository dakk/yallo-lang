open Yallo
open Errors

let opt = Compiler.{
  contract = None;
  out_lang = None;
  print_pt = false;
  print_ast = false;
  verbose = false;
}

let compile exc path cname _ = 
  let compile_failure = try (Compiler.compile path { opt with contract=cname }; None) with 
  | f -> Some(f) in

  (match compile_failure, exc with 
  | None, None -> ()
  | Some(SyntaxError(_)), Some(SyntaxError(_)) -> ()
  | Some(TypeError(_)), Some(TypeError(_)) -> ()
  | Some(DuplicateSymbolError(_)), Some(DuplicateSymbolError(_)) -> ()
  | Some(DeclarationError(_)), Some(DeclarationError(_)) -> ()
  | Some(f), _ -> failwith @@ "Invalid" ^ Printexc.to_string f
  | None, Some(e) -> failwith @@ "Expected an exception, none catched: " ^ Printexc.to_string e)




let () =
  Alcotest.run "yallo" [
    "misc", [
      "pragma", `Quick, compile None "test/misc/pragma.yallo" None;
      "expr", `Quick, compile None "test/misc/expr.yallo" None;
      "literal", `Quick, compile None "test/misc/literal.yallo" None;
      "record", `Quick, compile None "test/misc/record.yallo" None;
      "statements", `Quick, compile None "test/misc/statements.yallo" None;
      "typemod", `Quick, compile None "test/misc/typemod.yallo" None;
      "types", `Quick, compile None "test/misc/types.yallo" None;
    ];
    "expr", [
      "assoc", `Quick, compile None "test/expr/assoc.yallo" None;
      "match", `Quick, compile None "test/expr/match.yallo" None;
      "tuple_destruct", `Quick, compile None "test/expr/tuple_destruct.yallo" None;
    ];
    "const", [
      "cont", `Quick, compile None "test/const/cont.yallo" None;
      "numeric", `Quick, compile None "test/const/numeric.yallo" None;
      "string", `Quick, compile None "test/const/string.yallo" None;
      "lambda", `Quick, compile None "test/const/lambda.yallo" None;
      "lambda_fail", `Quick, compile (Some(TypeError(""))) "test/const/lambda_fail.yallo" None;
      "expr", `Quick, compile None "test/const/expr.yallo" None;
      "let_expr", `Quick, compile None "test/const/let_expr.yallo" None;
      "enum", `Quick, compile None "test/const/enum.yallo" None;
      "crypto_f", `Quick, compile None "test/const/crypto_f.yallo" None;
      "infer", `Quick, compile None "test/const/infer.yallo" None;
      "let_infer", `Quick, compile None "test/const/let_infer.yallo" None;
    ];
    "import", [
      "interface", `Quick, compile None "test/import/interface.yallo" None;
      "type", `Quick, compile None "test/import/type.yallo" None;
    ];
    "interface", [
      "i1", `Quick, compile None "test/interface/i1.yallo" None;
      "extend", `Quick, compile None "test/interface/extend.yallo" None;
      "empty", `Quick, compile None "test/interface/empty.yallo" None;
      "duplicate_fail", `Quick, compile (Some(DuplicateSymbolError(""))) "test/interface/duplicate_fail.yallo" None;
      "dup_entry_fail", `Quick, compile (Some(DeclarationError(""))) "test/interface/dup_entry_fail.yallo" None;
    ];
    "function", [
      "wrong_return_type", `Quick, compile (Some(SyntaxError(""))) "test/function/wrong_return_type.yallo" None;
      "base_fun", `Quick, compile None "test/function/base_fun.yallo" None;
    ];
    "contract", [
      "itoken", `Quick, compile None "test/contract/itoken.yallo" None;
      "loan", `Quick, compile None "test/contract/loan.yallo" None;
      "token", `Quick, compile None "test/contract/token.yallo" None;
      "token_create", `Quick, compile None "test/contract/token_create.yallo" None;
      "token_using", `Quick, compile None "test/contract/token_using.yallo" None;
      "crec", `Quick, compile None "test/contract/crec.yallo" None;
      "cenum", `Quick, compile None "test/contract/cenum.yallo" None;
      "ctor_fail", `Quick, compile (Some(DeclarationError(""))) "test/contract/ctor_fail.yallo" None;
    ]
  ]